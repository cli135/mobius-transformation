import scipy as sp
import numpy as np

from matplotlib import pyplot as plt

def onStrip(start, end, width, p): # start and end of orthogonal line, query point p, line width
    if start[0] == end[0]: # parallel to y-axis
        dist = p[0] - start[0] 
    elif start[1] == end[1]: # parrallel to x-axis
        dist = p[1] - start[1]
    else:
        raise Exception("query line is not orthogonal to either x or y axis")
    return (abs(dist) - width/2) <= 1e-8 

# def sample_grid(p, line_list, width, half_edge_length):
#     if (abs(p[0]) > half_edge_length + width/2) or (abs(p[1]) > half_edge_length+width/2):
#         return 0
#     for line in line_list:
#         if onStrip(line[0], line[1], width, p):
#             #print(line[0], line[1])
#             return 1
#     return 0

# better version
def sample_grid(p, grid_size, width, half_edge_length):
    if (abs(p[0]) > half_edge_length + width/2) or (abs(p[1]) > half_edge_length+width/2):
        return 0
    for i in range(2):
        v = grid_size * p[i] / half_edge_length 
        if abs(round(v) - v) < width * 0.5 * grid_size / half_edge_length:
            return 1
    return 0

# doing projection between x^2+y^2+(z-z_c)^2=1 sphere to the z=0 plane, requires z_c > 0
# these functions are all global to global with primitives defined above
def sProj(x, y, z, z_c=1): # stereographic projection
    # using the second formulation from https://en.wikipedia.org/wiki/Stereographic_projection
    # actually, see my ipad note instead of wikipedia
    #return [2*x/(2-z), 2*y/(2-z)]
    return [(z_c+1)*x/(z_c+1-z), (z_c+1)*y/(z_c+1-z)] #TODO: numerical stability for checking zero?

#doing projection from z=0 plane to  x^2+y^2+(z-z_c)^2=1 sphere
def invSProj(X, Y, z_c = 1):
    A = 1 / ((1+z_c)**2 + X**2 + Y**2)
    return [(1+z_c) * 2 * A * X, (1+z_c) * 2 * A * Y, z_c - ((1+z_c)**2 - X ** 2 - Y ** 2 ) * A]

def mapMoebius(p, alpha=0, beta=0, p_offset=[0, 0, 1], verbose=0): 
    # this is a 2D to 2D function such that given (X,Y), we should sample from (X',Y') which is the location given by Moebius transformation 
    # would be a functor(?) in Ocaml
    # step 1. invSProj (X,Y) -> (x1, y1, z1) centered at p_offset
    x1, y1, z1 = invSProj(p[0]-p_offset[0], p[1]-p_offset[1], p_offset[2]) # global
    x1_, y1_, z1_ = x1, y1, z1 - p_offset[2] # local
    if verbose:
        print(x1_, y1_, z1_)
    # step 2. (x1,y1,z1)->(x2, y2, z2) rotate and translate back
    p_loc = np.array([[x1_], [y1_], [z1_]])
    # the following part is basically eular angle but not so sure how that should be done in Ocaml
    r_alpha = np.array([[np.cos(-alpha), -np.sin(-alpha), 0], #rot mat to z-axis of -alpha angle
                    [np.sin(-alpha), np.cos(-alpha),  0],
                    [0,                           0, 1]])
    r_beta = np.array([[np.cos(beta), 0, np.sin(beta)], #rot mat to y-axis of beta angle 
                    [0 , 1,  0],
                    [-np.sin(beta),0, np.cos(beta)]])
    p_loc_rot = np.matmul(r_beta, np.matmul(r_alpha,p_loc)) # I think the order matters?
    x2, y2, z2 = p_loc_rot.flatten() 
    z2 += 1 # global
    if verbose:
        print(x2, y2, z2)
    # step 3. sProj (x2, y2, z2) -> (X', Y') the location to sample from
    X_p, Y_p = sProj(x2, y2, z2)
    return X_p, Y_p

rad = lambda x: x/180.0*np.pi

def pointOnSphere(start, dir_unit): # ray sphere intersection
    D = np.dot(-start, dir_unit)
    d = np.sqrt(max(np.linalg.norm(start)**2-D**2,1e-6)) # to avoid numerical issue 
    if (d > 1): 
        return np.zeros(3)
    l = np.sqrt(1-d**2)
    return unit_vec(start + (D-l) * dir_unit) #local

def mapSphereSample(p, alpha=0, beta=0): #given a point on sphere(local coordinates), found where it should map from(global coordinate)
    r_alpha = np.array([[np.cos(-alpha), -np.sin(-alpha), 0], #rot mat to z-axis of -alpha angle
                    [np.sin(-alpha), np.cos(-alpha),  0],
                    [0,                           0, 1]])
    r_beta = np.array([[np.cos(beta), 0, np.sin(beta)], #rot mat to y-axis of beta angle
                    [0 , 1,  0],
                    [-np.sin(beta),0, np.cos(beta)]])
    p_loc_rot = np.matmul(r_beta, np.matmul(r_alpha,p))
    x2, y2, z2 = p_loc_rot.flatten() 
    z2 += 1 # global
    # step 3. sProj (x2, y2, z2) -> (X', Y') the location to sample from
    X_p, Y_p = sProj(x2, y2, z2)
    return X_p, Y_p

unit_vec = lambda p: p / np.linalg.norm(p)

def planeIntersection(start, dir_unit): 
    t = -start[2] / dir_unit[2]
    return start + t * dir_unit

def computeDir(view_direction):
    p_unit = unit_vec(np.array(view_direction)) # we assume view_direction is not along z axis
        
    if p_unit[2] < 1e-10:
        up_dir = np.array([0,0,1])
    else:
        up_dir = unit_vec(np.array([0,0,1/p_unit[2]]) - p_unit)
    right_dir = -np.cross(p_unit, up_dir)
    return -p_unit, right_dir, up_dir

def getLines(grid_size=2, half_edge_length=1):
    # this part is probably going to be a functor in Ocaml
    x_l = np.array([1.0/grid_size * i for i in range(grid_size+1)])
    x_l = (x_l *2 - 1) * half_edge_length

    line_list = []
    
    for i in range(grid_size+1):
        line_list.append([[x_l[i], -half_edge_length],[x_l[i], half_edge_length]]) # lines that are parallel to y-axis
        line_list.append([[-half_edge_length, x_l[i]],[half_edge_length, x_l[i]]]) # lines that are parallel to x-axis
    line_list = np.array(line_list)
    return line_list

#-------> increase of j
#|   (0,0)  (0,1)
#|   (1,0)  (1,1)
#|/ increase of i

def getImage(sample_method, cameraoffset= 1, view_direction = [1, -1, 1], alpha=0, beta=0, p_offset=[0,0,1], image_w=200, viewsize = 8, plane_bd = 4, half_edge_length = 1, width = 0.1, grid_size = 2):
    img = np.empty([image_w,image_w])
    for i in range(image_w):
        for j in range(image_w):
            img[i,j] = sample_method(i, j, grid_size, cameraoffset=cameraoffset, view_direction=view_direction, alpha=alpha, beta=beta,  image_w=image_w, p_offset=p_offset, viewsize=viewsize, plane_bd=plane_bd, half_edge_length = half_edge_length, width = width)
    return img

def sample_plane(i, j, grid_size, cameraoffset= 1, view_direction = [1, -1, 1], alpha=0, beta=0, p_offset=[0,0,1], image_w=200 ,viewsize = 8, plane_bd = 4, half_edge_length = 1, width = 0.1):
    p = [-viewsize + 2.0 * j / image_w * viewsize , viewsize - 2.0 * i / image_w * viewsize]
    #print(image_w)
    #p = [2.0 * j / image_w , - 2.0 * i ]
    #print(p)
    p = mapMoebius(p, alpha, beta, p_offset)
    return sample_grid(p, grid_size, width, half_edge_length)

def sample_sphere(i, j, grid_size, cameraoffset= 1, view_direction = [1, -1, 1], alpha=0, beta=0, p_offset=[0,0,1], image_w=200 ,viewsize = 8, plane_bd = 4, half_edge_length = 1, width = 0.1):
    forward_dir, right_dir, up_dir = computeDir(view_direction) # these are all unit vectors
    p_start = forward_dir * 2 - (2*i - image_w) / image_w * up_dir + (2*j - image_w) / image_w * right_dir
    if i==100 and j==100:
        print("prepare to stop")
    p_on_sphere = pointOnSphere(p_start, forward_dir)
    #print(p_start)
    if np.all(p_on_sphere == 0):
        return 0
    p_on_plane = mapSphereSample(p_on_sphere, alpha, beta) # TODO: supersampling?
    c = sample_grid(p_on_plane, grid_size, width, half_edge_length)
    if c == 0:
        return 0.2
    return c

def sample_all(i, j, grid_size, cameraoffset= 0, view_direction = [0, -1, 1], alpha=0, beta=0, p_offset=[0,0,1], image_w=200 ,viewsize = 4, plane_bd = 4, half_edge_length = 1, width = 0.1):
    forward_dir, right_dir, up_dir = computeDir(view_direction) # these are all unit vectors
    p_start = np.array([0,0,cameraoffset])- 1.5 * forward_dir - (2*i - image_w) / image_w * up_dir * viewsize + (2*j - image_w) / image_w * right_dir * viewsize
    p_start_loc = p_start - np.array(p_offset)
    p_on_sphere = pointOnSphere(p_start_loc, forward_dir)
    if np.all(p_on_sphere == 0):
        # sample from plane
        p_on_plane = planeIntersection(p_start, forward_dir)
        if (abs(p_on_plane[0]) > plane_bd or abs(p_on_plane[1]) > plane_bd):
            return 0
        c = sample_grid(mapMoebius(p_on_plane,alpha, beta, p_offset), grid_size, width, half_edge_length)
        if c==0:
            return 0.4
    else:
        p_on_plane = mapSphereSample(p_on_sphere, alpha, beta) # TODO: supersampling?
        c = sample_grid(p_on_plane, grid_size, width, half_edge_length)
        if c==0:
            return 0.2
    return c


if __name__ == "__main__":
    for s in [sample_plane, sample_sphere, sample_all]:
        img = getImage(s, cameraoffset = 1, view_direction = [1,-2,1],alpha = rad(45), beta=rad(-30), p_offset =[0,1,3], image_w=200, viewsize=4, grid_size=2, half_edge_length=2, width = 0.2)
        plt.imshow(img, cmap='gray')
        plt.show()