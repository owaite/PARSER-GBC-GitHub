import os
from tqdm import tqdm
import numpy as np

basecmd = 'exiv2 -M"reg Camera http://pix4d.com/camera/1.0/"'
basecmd2 = 'exiv2 -M"reg MicaSense http://micasense.com/MicaSense/1.0/"'
basecmd3 = 'exiv2 -M"reg DLS http://micasense.com/DLS/1.0/"'


def makeValue(tag, basecmd,val,file,mode='f',delete=False):
    if delete:
        cmd = basecmd+' -M "del {:}"'.format(tag)
    else:
        cmd = basecmd
    if mode == 'f':
        cmd += ' -M "add {:} {:.10f}"'.format(tag,val)
    elif mode == 'e':
        cmd += ' -M "add {:} {:e}"'.format(tag,val)
    elif mode == 'd':
        cmd += ' -M "add {:} {:d}"'.format(tag,val)
    
    cmd +=' {:}'.format(file)
    return cmd



def edit_data(paths,kwd):
    for nb in ['1','2','3','4','5','6','7','8','9','10']:
        
        dls2_hori=np.load('./out/'+kwd+'/'+kwd+'_'+nb+'_horizontal_dls2.npy')
        dls2_scattered=np.load('./out/'+kwd+'/'+kwd+'_'+nb+'_scattered_dls2.npy')
        dls2_direct=np.load('./out/'+kwd+'/'+kwd+'_'+nb+'_direct_dls2.npy')
        dls2_ssa=np.load('./out/'+kwd+'/'+kwd+'_'+nb+'_sun_sensor_angle_dls2.npy')
        
        hori_m=np.load('./out/'+kwd+'/'+kwd+'_'+nb+'_horizontal_manual.npy')
        scat_m=np.load('./out/'+kwd+'/'+kwd+'_'+nb+'_scattered_manual.npy')
        dire_m=np.load('./out/'+kwd+'/'+kwd+'_'+nb+'_direct_manual.npy')
        ssa_m=np.load('./out/'+kwd+'/'+kwd+'_'+nb+'_sun_sensor_angle.npy')
    
        i=0
        for path in paths:
        
            files=os.listdir(path)
            files=[file for file in files if 'tif' in file]
            files=[file for file in files if (os.path.basename(file).split("_")[2].split(".")[0])==nb]
            imageNames=[path+file for file in files]
           
            for imageName in tqdm(imageNames):
                
                f_in=imageName
        
                cmd = makeValue('Xmp.DLS.HorizontalIrradianceDLS2',basecmd3,dls2_hori[i]*1e2,f_in,mode='f',delete=True)
                os.system(cmd)
                cmd = makeValue('Xmp.DLS.DirectIrradianceDLS2',basecmd3,dls2_direct[i]*1e2,f_in,mode='f',delete=True)
                os.system(cmd)
                cmd = makeValue('Xmp.DLS.ScatteredIrradianceDLS2',basecmd3,dls2_scattered[i]*1e2,f_in,mode='f',delete=True)
                os.system(cmd)
                cmd = makeValue('Xmp.DLS.SunSensorAngleDLS2',basecmd3,dls2_ssa[i],f_in,mode='f',delete=True)
                os.system(cmd)
                
                cmd = makeValue('Xmp.DLS.HorizontalIrradiance',basecmd3,hori_m[i]*1e2,f_in,mode='f',delete=True)
                os.system(cmd)
                cmd = makeValue('Xmp.DLS.DirectIrradiance',basecmd3,dire_m[i]*1e2,f_in,mode='f',delete=True)
                os.system(cmd)
                cmd = makeValue('Xmp.DLS.ScatteredIrradiance',basecmd3,scat_m[i]*1e2,f_in,mode='f',delete=True)
                os.system(cmd)
                cmd = makeValue('Xmp.DLS.SunSensorAngle',basecmd3,ssa_m[i],f_in,mode='f',delete=True)
                os.system(cmd)
                
                i+=1
