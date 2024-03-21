import cv2
import matplotlib.pyplot as plt
import numpy as np
import os,glob
import math
import micasense.metadata as metadata
from micasense.utils import vignette_map 
import exiftool
import os
from tqdm import tqdm
import pandas as pd
import seaborn as sns
import micasense.dls as dls
import micasense.capture as capture
import micasense.utils as msutils






def create_folders(kwd):
    if not os.path.exists('./out/'+kwd):
        os.mkdir('./out/'+kwd)
    if not os.path.exists('./out/figures/'+kwd):
        os.mkdir('./out/figures/'+kwd)

def flag_parts(dls2_spectral,dls2_hori):
    flag_close=np.zeros(dls2_hori.size).astype(int)
    flag_close[np.abs(dls2_spectral-dls2_hori)/dls2_spectral<0.3]=1
    return flag_close

def get_corrected_values(paths,kwd,ratio,plot=True):
    for nb in ['1','2','3','4','5','6','7','8','9','10']:
        
        dir_diff_ratio = ratio
        
        dls2_hori=[]
        dls2_scattered=[]
        dls2_spectral=[]
        dls2_fresnel=[]
        ssa=[]
        ssam=[]
        se=[]
    
        for path in paths:
        
            files=os.listdir(path)
            files=[file for file in files if 'tif' in file]
            files=[file for file in files if (os.path.basename(file).split("_")[2].split(".")[0])==nb]
            imageNames=[path+file for file in files]
           
            for imageName in tqdm(imageNames):
                '''
                GET RADIANCE IMAGES DATA
                '''
                meta = metadata.Metadata(imageName)
        
                cap = capture.Capture.from_filelist([imageName])
                dls2_hori.append(meta.horizontal_irradiance())
                dls2_scattered.append(meta.scattered_irradiance())
                dls2_spectral.append(meta.spectral_irradiance())
                 
                ssa_1 = meta.estimated_direct_vector()[2]
                ssa_2 = np.arccos(-1 * ssa_1)*180/np.pi
                ssa.append(ssa_2)
    
                dls_orientation_vector = np.array([0,0,-1])
                # compute sun orientation and sun-sensor angles
                (
                    sun_vector_ned,    # Solar vector in North-East-Down coordinates
                    sensor_vector_ned, # DLS vector in North-East-Down coordinates
                    sun_sensor_angle_manual,  # Angle between DLS vector and sun vector
                    solar_elevation,   # Elevation of the sun above the horizon
                    solar_azimuth,     # Azimuth (heading) of the sun
                ) = dls.compute_sun_angle(cap.location(),
                                      cap.dls_pose(),
                                      cap.utc_time(),
                                      dls_orientation_vector)
                ssam.append(sun_sensor_angle_manual*180/np.pi)
                se.append(solar_elevation*180/np.pi)
                dls2_fresnel.append(dls.fresnel(sun_sensor_angle_manual))
    
        dls2_hori=np.asarray(dls2_hori)
        dls2_scattered=np.asarray(dls2_scattered)
        dls2_spectral=np.asarray(dls2_spectral)
        dls2_fresnel=np.asarray(dls2_fresnel)
        dls2_direct=dls2_hori-dls2_scattered
        ssam=np.asarray(ssam)
        ssa=np.asarray(ssa)
        se=np.asarray(se)
    
        ###
        flag_close=flag_parts(dls2_spectral,dls2_hori)

#         dir_diff_ratio=np.nanmedian(dls2_direct[flag_close==1]/dls2_scattered[flag_close==1])
        #dir_diff_ratio=np.nanmedian(dls2_scattered[flag_close==1]/dls2_direct[flag_close==1])
    
#         dls2_spectral=dls2_spectral/dls2_fresnel 
#         hori_m=dls2_spectral*(np.cos(90-se*np.pi/180)+dir_diff_ratio)/(np.cos(ssam*np.pi/180)+dir_diff_ratio)
#         dir_m=dls2_spectral/(np.cos(ssam*np.pi/180)+dir_diff_ratio)*np.cos(90-se*np.pi/180)
#         sca_m=hori_m-dir_m
    
        dls2_spectral=dls2_spectral/dls2_fresnel 
        hori_m=dls2_spectral*(np.cos((90-se)*np.pi/180)+dir_diff_ratio)/(np.cos(ssam*np.pi/180)+dir_diff_ratio)
        dir_m=dls2_spectral/(np.cos(ssam*np.pi/180)+dir_diff_ratio)#*np.cos((90-se)*np.pi/180)
        # we are trying without this multiplier, Thomas?!?!
        sca_m=hori_m-dir_m
        
#         hori_m_rads=dls2_spectral*(np.cos(90-se*np.pi/180)+dir_diff_ratio)/(np.cos(ssam*np.pi/180)+dir_diff_ratio)
#         dir_m_rads=dls2_spectral/(np.cos(ssam*np.pi/180)+dir_diff_ratio)*np.cos(90-se*np.pi/180)
        
        '''
        saves numpy files containing corrected data irradiance data, associated fresnel correction values, and associated sun sensor angles
        '''
    
        #save
        np.save('./out/'+kwd+'/'+kwd+'_'+nb+'_horizontal_dls2.npy',dls2_hori)
        np.save('./out/'+kwd+'/'+kwd+'_'+nb+'_direct_dls2.npy',dls2_direct)
        np.save('./out/'+kwd+'/'+kwd+'_'+nb+'_scattered_dls2.npy',dls2_scattered)
        
        np.save('./out/'+kwd+'/'+kwd+'_'+nb+'_horizontal_manual.npy',hori_m)
        np.save('./out/'+kwd+'/'+kwd+'_'+nb+'_direct_manual.npy',dir_m)
        np.save('./out/'+kwd+'/'+kwd+'_'+nb+'_scattered_manual.npy',sca_m)
        
        np.save('./out/'+kwd+'/'+kwd+'_'+nb+'_fresnel.npy',dls2_fresnel)
        np.save('./out/'+kwd+'/'+kwd+'_'+nb+'_sun_sensor_angle.npy',ssam)
        np.save('./out/'+kwd+'/'+kwd+'_'+nb+'_sun_sensor_angle_dls2.npy',ssa)
        
#         print("horizontal manual (radians) = ")
#         print(hori_m_rads)
        
#         print("horizontal manual = ")
#         print(hori_m)
                
#         print("direct manual (radians) = ")
#         print(dir_m_rads)
        
#         print("direct manual = ")
#         print(dir_m)
        
        
        if plot==True:
            '''
            plots and saves figures
            '''
            #fig,ax=plt.subplots(2,2)
            #ax[0,0].hist(dls2_hori[flag_close==1],bins=100)
            #ax[0,0].set_xlabel('hori')
            #ax[1,0].hist(dls2_scattered[flag_close==1],bins=100)
            #ax[1,0].set_xlabel('scattered')
            #ax[0,1].hist(dls2_direct[flag_close==1],bins=100)
            #ax[0,1].set_xlabel('direct')
            #ax[1,1].hist(dls2_direct[flag_close==1]/dls2_scattered[flag_close==1],bins=100)
            #ax[1,1].set_xlabel('ratio')
            #fig.savefig('./out/figures/'+kwd+'/'+kwd+'_'+nb+'_histograms.png')
    
            fig,ax=plt.subplots()
            ax.scatter(np.arange(dls2_hori.size),dls2_hori,label='hor',marker='+')
            ax.scatter(np.arange(dls2_hori.size),dls2_scattered,label='scat',marker='+')
            ax.scatter(np.arange(dls2_hori.size),dls2_spectral,label='spec',marker='+')
            ax.fill_between(np.arange(dls2_hori.size),dls2_spectral*0.7,dls2_spectral*1.3,color='lightgray',zorder=0)
            ax.plot(np.arange(dls2_hori.size),hori_m,label='manual hor',color='k')
            ax.legend()
            fig.savefig('./out/figures/'+kwd+'/'+kwd+'_'+nb+'_plot_time.png')
        
