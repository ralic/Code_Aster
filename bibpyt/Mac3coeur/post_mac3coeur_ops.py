# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
# (AT YOUR OPTION) ANY LATER VERSION.
#
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.
#
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
# ======================================================================
# person_in_charge: samuel.geniaut at edf.fr
import aster_core
from mac3coeur_coeur import CoeurFactory

def makeXMGRACEjeu(unit,post,coeur,valjeuac,valjeucu):
    def computeColor(value):
        valmin=0.
        valmax=0.7
        if (value <= valmin):
            redF   = 255
            greenF =   0
            blueF  =   0
            lame   =  'c'
            size   =   0.8
        elif (value > valmax):
            redF   = 0
            greenF = 0
            blueF  = 0
            lame   = '%.1f'%value
            size   =   0.6
        else :
            redF   =   0
            greenF =   0
            blueF  = 255
            lame   = '%.1f'%value
            size   =   0.6

        return (redF,greenF,blueF,lame,size)

    def NodePos(position):
        pos2=position[0:1]
        pos1=position[1:2]

        x = coeur.ALPHAMAC.index(pos1) - (len(coeur.ALPHAMAC)-1)/2.
        y = len(coeur.ALPHAMAC) - coeur.ALPHAMAC.index(pos2) - 1. - (len(coeur.ALPHAMAC)-1)/2.

        return (x,y)

    def NodePosCu(position):
        x = 0.
        y = 0.
        if (position=='W'):
           x = -0.5
        elif (position=='N'):
           y = +0.5
        elif (position=='E'):
           x = +0.5
        elif (position=='S'):
           y = -0.5

        return (x,y)

    POSITION = coeur.get_geom_coeur()
    filename = './fort.%d' %(unit)

    xmgrfile = open(filename, 'w')
    xmgrfile.write('@focus on\n@g0 on\n@with g0\n')
    xmgrfile.write('@VIEW 0.1,0.1,0.85,0.85\n')
    for val_abs in range(0,len(coeur.ALPHABET)):
        xmgrfile.write('@with string\n@string on\n@string loctype world\n@string color ' \
                       '(0,0,0)\n@string char size 0.8\n@string just 2\n@string %f, %f\n' \
                       '@string def \"%s\"\n' \
                       % (val_abs - (len(coeur.ALPHAMAC)-1)/2., (len(coeur.ALPHAMAC)-1)/2.+1.2,
                          coeur.ALPHABET[val_abs]))
    for val_ord in range(0,len(coeur.NumV)):
        xmgrfile.write('@with string\n@string on\n@string loctype world\n@string color (0,0,0)\n' \
                       '@string char size 0.8\n@string just 2\n@string %f, %f\n' \
                       '@string def \"%s\"\n' \
                       % (-(len(coeur.ALPHAMAC)-1)/2.-1.5,  val_ord - (len(coeur.ALPHAMAC)-1)/2.-0.2,
                          coeur.NumV[len(coeur.NumV)-1-val_ord]))
    xmgrfile.write('@kill s0\n@s0 line pattern 0\n@s0 symbol fill pattern 0\n%f %f\n%f %f\n' \
        %(-(len(coeur.ALPHAMAC)-1)/2.-1.02, -(len(coeur.ALPHAMAC)-1)/2.-0.4,
          (len(coeur.ALPHAMAC)-1)/2.+0.5,(len(coeur.ALPHAMAC)-1)/2.+1.05))

    ind=0
    for k in POSITION:
       ind=ind+1
       y=coeur.ALPHAMAC.index(k[0])-(len(coeur.ALPHAMAC)-1)/2.
       x=coeur.ALPHAMAC.index(k[1])-(len(coeur.ALPHAMAC)-1)/2.
       xmgrfile.write('@kill s%d\n@s%d symbol 2\n@s%d symbol pattern 1\n@s%d symbol size 0.4\n' \
                      '@s%d symbol color 1\n@s%d symbol fill pattern 1\n@s%d symbol fill color 1\n' \
                      '@type xy\n%10.8f %10.8f\n' %(ind,ind,ind,ind,ind,ind,ind,x,y))

    for name in valjeucu.keys():
        position1 = name[3:5]
        position2 = name[6:7]
        (x1,y1) = NodePos(position1)
        (x2,y2) = NodePosCu(position2)
        if (post=='MAXI') :
           (redF,greenF,blueF,lame,size) = computeColor(max(valjeucu[name]))
           titre = 'maximaux'
        elif (post=='MINI') :
           (redF,greenF,blueF,lame,size) = computeColor(min(valjeucu[name]))
           titre = 'minimaux'
        else :
           (redF,greenF,blueF,lame,size) = computeColor(valjeucu[name][post-1])
           titre = 'au niveau des grilles %s'%post
        xmgrfile.write('@with string\n@string on\n@string loctype world\n' \
            '@string color (%d,%d,%d)\n@string char size %f\n@string just 2\n@string %f, %f\n' \
            '@string def \"%s\"\n' %(redF,greenF,blueF,size,(x1+x2),(y1+y2)-0.1,lame))

    if len(valjeuac) != 0 :
      for name in valjeuac.keys():
          position1 = name[4:6]
          position2 = name[6:8]
          (x1,y1) = NodePos(position1)
          (x2,y2) = NodePos(position2)
          if (post=='MAXI') :
              (redF,greenF,blueF,lame,size) = computeColor(max(valjeuac[name]))
          elif (post=='MINI') :
              (redF,greenF,blueF,lame,size) = computeColor(min(valjeuac[name]))
          else :
              (redF,greenF,blueF,lame,size) = computeColor(valjeuac[name][post-1])
          xmgrfile.write('@with string\n@string on\n@string loctype world\n' \
            '@string color (%d,%d,%d)\n@string char size %f\n@string just 2\n@string %f, %f\n' \
            '@string def \"%s\"\n' %(redF,greenF,blueF,size,(x1+x2)/2.0,(y1+y2)/2.0-0.1,lame))

    xmgrfile.write('&\n@xaxis ticklabel off\n@yaxis ticklabel off\n@xaxis tick off\n' \
        '@yaxis tick off\n@subtitle \"Jeux %s entre les ACs du Coeur (en mm)"\n' \
        '@DEVICE \"JPEG\" PAGE SIZE 1200,1200\n@autoscale\n@redraw\n'%(titre))
    xmgrfile.close()

def makeXMGRACEdef_amp(unit,post,coeur,valdefac):

    def computeColor(value):
        valmin= 0.
        valmax=20.
        if (value <= 10.):
            redF   =   0
            greenF = 255*value/10
            blueF  = 255*(1-value/10)
            size   =   value/20.
        elif (value <= 20.):
            redF   = 255*(value-10)/10
            greenF = 255*(1-(value-10)/10)
            blueF  = 0
            size   =   value/20.
        else :
            redF   =   0
            greenF =   0
            blueF  =   0
            size   =   1.0

        return (redF,greenF,blueF,size)

    filename = './fort.%d' %(unit)

    xmgrfile = open(filename, 'w')
    xmgrfile.write('@focus on\n@g0 on\n@with g0\n')
    xmgrfile.write('@VIEW 0.1,0.1,0.85,0.85\n')
    for val_abs in range(0,len(coeur.ALPHABET)):
        xmgrfile.write('@with string\n@string on\n@string loctype world\n@string color (0,0,0)\n' \
            '@string char size 0.8\n@string just 2\n@string %f, %f\n@string def \"%s\"\n' \
            % (val_abs - (len(coeur.ALPHAMAC)-1)/2., (len(coeur.ALPHAMAC)-1)/2.+1.2,
               coeur.ALPHABET[val_abs]))
    for val_ord in range(0,len(coeur.NumV)):
        xmgrfile.write('@with string\n@string on\n@string loctype world\n@string color (0,0,0)\n' \
            '@string char size 0.8\n@string just 2\n@string %f, %f\n@string def \"%s\"\n' \
            % (-(len(coeur.ALPHAMAC)-1)/2.-1.5,  val_ord - (len(coeur.ALPHAMAC)-1)/2.-0.2,
             coeur.NumV[len(coeur.NumV)-1-val_ord]))
    xmgrfile.write('@kill s0\n@s0 line pattern 0\n@s0 symbol fill pattern 0\n%f %f\n%f %f\n' \
        % (-(len(coeur.ALPHAMAC)-1)/2.-1.02, -(len(coeur.ALPHAMAC)-1)/2.-0.4,
            (len(coeur.ALPHAMAC)-1)/2.+0.5,(len(coeur.ALPHAMAC)-1)/2.+1.05))
    ind=0
    for name in valdefac.keys():
        ind=ind+1
        position2 = name[0]
        position1 = name[2]
        x = coeur.ALPHAMAC.index(position1) - (len(coeur.ALPHAMAC)-1)/2.
        y = len(coeur.ALPHAMAC) - coeur.ALPHAMAC.index(position2) - 1. - (len(coeur.ALPHAMAC)-1)/2.
        if (post=='MAXI') :
            (redF,greenF,blueF,size) = computeColor(max(valdefac[name]))
            titre = 'maximale'
        elif (post=='MINI') :
            (redF,greenF,blueF,size) = computeColor(min(valdefac[name]))
            titre = 'minimale'
        else :
            (redF,greenF,blueF,size) = computeColor(valdefac[name][post-1])
            titre = 'au niveau des grilles %s'%post
        xmgrfile.write('@kill s%d\n@s%d symbol fill pattern %d\n' \
            '@s%d symbol fill color (%d, %d, %d)\n@s%d symbol 1\n@s%d symbol size %f\n' \
            '@s%d symbol color (%d, %d, %d)\n%d %d\n' % (ind,ind,1,ind,redF,greenF,blueF,ind,
                ind,2.*size,ind,redF,greenF,blueF,x,y))

    xmgrfile.write('&\n@xaxis ticklabel off\n@yaxis ticklabel off\n@xaxis tick off\n' \
        '@yaxis tick off\n@subtitle \"Deformation residuelle %s entre les ACs du Coeur (en mm)"\n' \
        '@DEVICE \"JPEG\" PAGE SIZE 1200,1200\n@autoscale\n@redraw\n'%(titre))
    xmgrfile.close()

def makeXMGRACEdef_mod(unit,post,coeur,valdefac):
    def computeColor(value):
        redF   =   0
        greenF =   0
        blueF  =   0

        return (redF,greenF,blueF,value)

    filename = './fort.%d' %(unit)

    xmgrfile = open(filename, 'w')
    xmgrfile.write('@focus on\n@g0 on\n@with g0\n')
    xmgrfile.write('@VIEW 0.1,0.1,0.85,0.85\n')
    for val_abs in range(0,len(coeur.ALPHABET)):
        xmgrfile.write('@with string\n@string on\n@string loctype world\n@string color (0,0,0)\n' \
            '@string char size 0.8\n@string just 2\n@string %f, %f\n@string def \"%s\"\n' \
            % (val_abs - (len(coeur.ALPHAMAC)-1)/2., (len(coeur.ALPHAMAC)-1)/2.+1.2,
               coeur.ALPHABET[val_abs]))
    for val_ord in range(0,len(coeur.NumV)):
        xmgrfile.write('@with string\n@string on\n@string loctype world\n@string color (0,0,0)\n' \
            '@string char size 0.8\n@string just 2\n@string %f, %f\n@string def \"%s\"\n' \
            % (-(len(coeur.ALPHAMAC)-1)/2.-1.5,  val_ord - (len(coeur.ALPHAMAC)-1)/2.-0.2,
               coeur.NumV[len(coeur.NumV)-1-val_ord]))
    xmgrfile.write('@kill s0\n@s0 line pattern 0\n@s0 symbol fill pattern 0\n%f %f\n%f %f\n' \
        % (-(len(coeur.ALPHAMAC)-1)/2.-1.02, -(len(coeur.ALPHAMAC)-1)/2.-0.4,
           (len(coeur.ALPHAMAC)-1)/2.+0.5,(len(coeur.ALPHAMAC)-1)/2.+1.05))
    ind=0
    for name in valdefac.keys():
        ind=ind+1
        position2 = name[0]
        position1 = name[2]
        x = coeur.ALPHAMAC.index(position1) - (len(coeur.ALPHAMAC)-1)/2.
        y = len(coeur.ALPHAMAC) - coeur.ALPHAMAC.index(position2) - 1. - (len(coeur.ALPHAMAC)-1)/2.
        if (post=='MAXI') :
            (redF,greenF,blueF,value) = computeColor(max(valdefac[name]))
            titre = 'maximales'
        elif (post=='MINI') :
            (redF,greenF,blueF,value) = computeColor(min(valdefac[name]))
            titre = 'minimales'
        else :
            (redF,greenF,blueF,value) = computeColor(valdefac[name][post-1])
            titre = 'au niveau des grilles %s'%post
        xmgrfile.write('@kill s%d\n@s%d symbol 2\n@s%d symbol pattern 1\n@s%d symbol size 2\n' \
            '@s%d symbol color 1\n@s%d symbol fill pattern 1\n' \
            '@s%d symbol fill color (248,248,252)\n@type xy\n%10.8f %10.8f\n' \
            % (ind,ind,ind,ind,ind,ind,ind,x,y))
        xmgrfile.write('@with string\n@string on\n@string loctype world\n' \
            '@string color(%d,%d,%d)\n@string char size %f\n@string just 2\n@string %f, %f\n' \
            '@string def \"%10.1f\"\n' % (redF,greenF,blueF,0.7,x-0.37,y-0.15,value))

    xmgrfile.write('&\n@xaxis ticklabel off\n@yaxis ticklabel off\n@xaxis tick off\n' \
        '@yaxis tick off\n@subtitle \"Module des deformations residuelles %s entre les ACs' \
        ' du Coeur (en mm)"\n@DEVICE \"JPEG\" PAGE SIZE 1200,1200\n@autoscale\n@redraw\n'%(titre))
    xmgrfile.close()

def makeXMGRACEdef_vec(unit,post,coeur,valdefac,valdirYac,valdirZac):
    def computeVector(value,Y,Z):
        valmin =    0.
        valmax =   20.
        Rvec   =   value/20.
        Xvec   =   1000.*Y/20.
        Yvec   =  -1000.*Z/20.

        return (Xvec,Yvec,Rvec)

    filename = './fort.%d' %(unit)

    xmgrfile = open(filename, 'w')
    xmgrfile.write('@focus on\n@g0 on\n@with g0\n')
    xmgrfile.write('@VIEW 0.1,0.1,0.85,0.85\n')
    for val_abs in range(0,len(coeur.ALPHABET)):
        xmgrfile.write('@with string\n@string on\n@string loctype world\n@string color (0,0,0)\n' \
            '@string char size 0.8\n@string just 2\n@string %f, %f\n@string def \"%s\"\n' \
            % (val_abs - (len(coeur.ALPHAMAC)-1)/2., (len(coeur.ALPHAMAC)-1)/2.+1.2,
                coeur.ALPHABET[val_abs]))
    for val_ord in range(0,len(coeur.NumV)):
        xmgrfile.write('@with string\n@string on\n@string loctype world\n@string color (0,0,0)\n' \
            '@string char size 0.8\n@string just 2\n@string %f, %f\n@string def \"%s\"\n' \
            % (-(len(coeur.ALPHAMAC)-1)/2.-1.5,  val_ord - (len(coeur.ALPHAMAC)-1)/2.-0.2,
                coeur.NumV[len(coeur.NumV)-1-val_ord]))
    xmgrfile.write('@kill s0\n@s0 line pattern 0\n@s0 symbol fill pattern 0\n%f %f\n%f %f\n' \
        % (-(len(coeur.ALPHAMAC)-1)/2.-1.02, -(len(coeur.ALPHAMAC)-1)/2.-0.4,
           (len(coeur.ALPHAMAC)-1)/2.+0.5,(len(coeur.ALPHAMAC)-1)/2.+1.05))
    ind=0
    for name in valdefac.keys():
        ind=ind+1
        position2 = name[0]
        position1 = name[2]
        x = coeur.ALPHAMAC.index(position1) - (len(coeur.ALPHAMAC)-1)/2.
        y = len(coeur.ALPHAMAC) - coeur.ALPHAMAC.index(position2) - 1. - (len(coeur.ALPHAMAC)-1)/2.
        if (post=='MAXI') :
            pos = valdefac[name].index(max(valdefac[name]))
            (Xvec,Yvec,Rvec) = computeVector(valdefac[name][pos],valdirYac[name][pos],valdirZac[name][pos])
            titre = 'maximale'
        elif (post=='MINI') :
            pos = valdefac[name].index(min(valdefac[name]))
            (Xvec,Yvec,Rvec) = computeVector(valdefac[name][pos],valdirYac[name][pos],valdirZac[name][pos])
            titre = 'minimale'
        else :
            (Xvec,Yvec,Rvec) = computeVector(valdefac[name][post-1],valdirYac[name][post-1],valdirZac[name][post-1])
            titre = 'au niveau des grilles %s'%post
        xmgrfile.write('@kill s%d\n@s%d errorbar on\n@s%d errorbar color (%d, %d, %d)\n' \
            '@s%d errorbar place both\n@s%d errorbar pattern 1\n@s%d errorbar size %f\n' \
            '@s%d errorbar linewidth %f\n@s%d errorbar linestyle 1\n' \
            '@s%d errorbar riser linewidth %f\n@s%d errorbar riser clip off\n@type xyvmap\n' \
            '%d %d %10.8f %10.8f\n' \
            % (ind,ind,ind,0,0,0,ind,ind,ind,0.6*Rvec,ind,2.5*Rvec,ind,ind,2.5*Rvec,
               ind,x,y,0.03*Xvec,0.03*Yvec))

    xmgrfile.write('&\n@xaxis ticklabel off\n@yaxis ticklabel off\n@xaxis tick off\n' \
        '@yaxis tick off\n@subtitle \"Orientation des deformations %s entre les ACs' \
        ' du Coeur (en mm)"\n@DEVICE \"JPEG\" PAGE SIZE 1200,1200\n@autoscale\n@redraw\n'%(titre))
    xmgrfile.close()


def makeXMGRACEdeforme(unit,name,typeAC,coeur,valdefac):
    ac = coeur.factory.get(typeAC)(coeur.typ_coeur)
    filename = './fort.%d' %(unit)

    xmgrfile = open(filename, 'w')
    xmgrfile.write('@focus off\n@g0 on\n@with g0\n@kill s0\n@s0 symbol 9\n' \
        '@s0 symbol linewidth 3\n@s0 linewidth 3\n')
    xmgrfile.write('@VIEW 0.1,0.1,0.85,0.85\n')

    for k in range(0,len(ac.altitude)):
        xmgrfile.write('@with string\n@string on\n@string loctype world\n' \
            '@string color (0,50,120)\n@string char size %f\n@string just 2\n' \
            '@string %f, %f\n@string def \"%10.1f\"\n' \
            % (0.7,valdefac[name][k]+1.0,ac.altitude[k]-0.035,valdefac[name][k]))
    for k in range(0,len(ac.altitude)):
        xmgrfile.write('%10.8f %10.8f\n' %(valdefac[name][k],ac.altitude[k]))

    xmgrfile.write('&\n@s2 on\n@with s2\n-20.0 -0.1\n&\n@s3 on\n@with s3\n20.0 4.6\n')
    xmgrfile.write('&\n@subtitle \"D\éform\ée de l\'assemblage %s (amplitudes (mm)/ha' \
        'uteur (m))"\n@DEVICE \"JPEG\" PAGE SIZE 1200,1200\n@autoscale\n@redraw\n'%(name))
    xmgrfile.close()

def PostForme(l_f,meth):
   """ post-traite la forme suivant une methode (Damac pour le moment) d'apres une liste de fleches en mm"""
   
   if meth=='DAMAC' : 
      A1 = abs(min(l_f))
      A2 = abs(max(l_f))
      if (A1 <= 0.5) or (A2 <=0.5) :
         forme = 'C'
      else :
         forme = 'S'
   
   assert(meth=='DAMAC')

   return forme


def post_mac3coeur_ops(self, **args):
    """Corps principal de la macro de post-traitement de MAC3COEUR"""
    import aster
    from Accas import _F
    from Utilitai.Utmess import  UTMESS
    from math import sqrt

    CREA_CHAMP    = self.get_cmd('CREA_CHAMP')
    CREA_TABLE    = self.get_cmd('CREA_TABLE')
    CALC_TABLE    = self.get_cmd('CALC_TABLE')
    EXTR_TABLE    = self.get_cmd('EXTR_TABLE')
    FORMULE       = self.get_cmd('FORMULE')
    DEFI_FICHIER  = self.get_cmd('DEFI_FICHIER')
    IMPR_TABLE    = self.get_cmd('IMPR_TABLE')

    self.set_icmd(1)
    _RESU       = self['RESULTAT']
    _typ_coeur  = self['TYPE_COEUR']
    POST_LAME   = self['LAME']
    POST_DEF    = self['DEFORMATION']
    _inst       = self['INST']

    datg = aster_core.get_option("repdex")
    coeur_factory = CoeurFactory(datg)
    _coeur = coeur_factory.get(_typ_coeur)('post', _typ_coeur, self, datg)

    if (POST_LAME != None) :
       valjeuac={}
       valjeucu={}
       post_table=0
       for attr in POST_LAME:
          _typ_post   = attr['FORMAT']
          if (_typ_post=='TABLE'):
             post_table=1

       _formule = FORMULE(NOM_PARA='V8',VALE='1000.*V8')

       UTMESS('I','COEUR0_5')
       k=0
       dim=len(_coeur.nomContactCuve)
       for name in _coeur.nomContactCuve:
          _TAB2 = CREA_TABLE(RESU=_F(RESULTAT=_RESU,NOM_CHAM='VARI_ELGA',NOM_CMP='V8',GROUP_MA=name,INST=_inst))
          _TAB2 = CALC_TABLE(reuse=_TAB2,TABLE=_TAB2,
                            ACTION = (_F(OPERATION='FILTRE',NOM_PARA='POINT',CRIT_COMP='EQ',VALE_I=1),
                                      _F(OPERATION='TRI',NOM_PARA='COOR_X',ORDRE='CROISSANT'),
                                      _F(OPERATION='OPER',FORMULE=_formule,NOM_PARA=name)))

          if (post_table==1):
             if (len(valjeuac)==0):
                _TAB3 = CALC_TABLE(TABLE  =  _TAB2,
                                   ACTION = (_F(OPERATION='EXTR',NOM_PARA=('COOR_X',name))))
             else:
                
                _TABTMP = CALC_TABLE(TABLE  =  _TAB2,
                                      ACTION = (_F(OPERATION='EXTR',NOM_PARA=('COOR_X',name))))
                _TAB3   = CALC_TABLE(TABLE=_TAB3,
                                      ACTION = (_F(OPERATION='COMB',TABLE=_TABTMP,NOM_PARA='COOR_X')))

          tab2 = _TAB2.EXTR_TABLE()
          tab2.Renomme(name, 'P_LAME')
          valjeucu[name] = tab2.P_LAME.values()
          k=k+1
      
       UTMESS('I','COEUR0_4')
       k=0
       dim=len(_coeur.nomContactAssLame)
       if dim != 0 :
          for name in _coeur.nomContactAssLame:
              _TAB1 = CREA_TABLE(RESU=_F(RESULTAT=_RESU,NOM_CHAM='VARI_ELGA',NOM_CMP='V8',GROUP_MA=name,INST=_inst))
              _TAB1 = CALC_TABLE(reuse=_TAB1,TABLE=_TAB1,
                                ACTION = (_F(OPERATION='FILTRE',NOM_PARA='POINT',CRIT_COMP='EQ',VALE_I=1),
                                          _F(OPERATION='TRI',NOM_PARA='COOR_X',ORDRE='CROISSANT'),
                                          _F(OPERATION='OPER',FORMULE=_formule,NOM_PARA=name)))
              if (post_table==1):
                _TABTMP = CALC_TABLE(TABLE  =  _TAB1,
                                    ACTION = (_F(OPERATION='EXTR',NOM_PARA=('COOR_X',name))))
                _TAB3   = CALC_TABLE(TABLE=_TAB3,
                                    ACTION = (_F(OPERATION='COMB',TABLE=_TABTMP,NOM_PARA='COOR_X')))
              tab1 = _TAB1.EXTR_TABLE()
              tab1.Renomme(name, 'P_LAME')
              valjeuac[name] = tab1.P_LAME.values()
              k=k+1

       for attr in POST_LAME:
          _num_grille = attr['NUME_GRILLE']
          _unit       = attr['UNITE']
          _extremum   = attr['TYPE_RESU']
          _typ_post   = attr['FORMAT']

          DEFI_FICHIER(ACTION='LIBERER',UNITE=_unit)

          if (_extremum == None):
             post  = _num_grille
             texte = 'sur la grille '+str(post)
          else:
             post = _extremum
             texte = 'sur la valeur '+post

          if (_typ_post=='GRACE'):
             makeXMGRACEjeu(_unit,post,_coeur,valjeuac,valjeucu)
          elif (_typ_post=='TABLE'):
             IMPR_TABLE(UNITE=_unit,TABLE=_TAB3)




    if (POST_DEF != None) :

       valdefac={}
       valdirYac={}
       valdirZac={}
       valrho={}
       valforme={}

       UTMESS('I','COEUR0_6')
       POSITION = _coeur.get_geom_coeur()
       k=0
       dim=len(POSITION)

       for name in POSITION:
          name_AC_aster = name[0]+'_'+name[1]
          _TAB1 = CREA_TABLE(RESU=_F(RESULTAT=_RESU,
                                     NOM_CHAM='DEPL',
                                     INST=_inst,
                                     NOM_CMP=('DY','DZ'),
                                     GROUP_MA='GR_'+name_AC_aster)
                             )
                             
          _TAB1 = CALC_TABLE(reuse=_TAB1,TABLE=_TAB1,
                             ACTION = (_F(OPERATION='TRI',NOM_PARA='COOR_X',ORDRE='CROISSANT'),
                                       ))

          
          tab1 = _TAB1.EXTR_TABLE()
          
          #IMPR_TABLE(TABLE=_TAB1)
          
          l_x_tmp  = tab1.COOR_X.values()
          l_dy_tmp = tab1.DY.values()
          l_dz_tmp = tab1.DZ.values()
          
          # on reduit les listes en supprimant les doublons
          nb_grilles = len(l_x_tmp)/4.
          assert (nb_grilles==int(nb_grilles))
          nb_grilles=int(nb_grilles)
          l_x  = [l_x_tmp[4*i] for i in range(nb_grilles)]
          l_dy = [l_dy_tmp[4*i] for i in range(nb_grilles)]
          l_dz = [l_dz_tmp[4*i] for i in range(nb_grilles)]
                    
          # on applique la formule des fleches
          l_fy=[]
          l_fz=[]
          for i in range(nb_grilles) :
            fy = l_dy[i] - l_dy[0] - (l_dy[-1]-l_dy[0])/(l_x[-1]-l_x[0]) * (l_x[i]-l_x[0])
            l_fy.append( fy )
            fz = l_dz[i] - l_dz[0] - (l_dz[-1]-l_dz[0])/(l_x[-1]-l_x[0]) * (l_x[i]-l_x[0])
            l_fz.append( fz )

          # on applique la formule de Rho
          rho=0.
          for i in range(nb_grilles):
             for j in range(nb_grilles):
                rho=max(rho, sqrt((l_fy[i]-l_fy[j])**2+(l_fz[i]-l_fz[j])**2) )

          # on applique la formule de la norme des fleches
          l_fnor = [sqrt(l_fy[i]**2+l_fz[i]**2) for i in range(nb_grilles)]

          # on passe en mm
          l_fy_mm = [1000.*fy for fy in l_fy]
          l_fz_mm = [1000.*fz for fz in l_fz]
          l_fnor_mm = [1000.*fnor for fnor in l_fnor]
          rho_mm = 1000.*rho
          
          # calcul des formes (important : a partir des fleches en mm )
          formeY = PostForme(l_fy_mm,'DAMAC')
          formeZ = PostForme(l_fz_mm,'DAMAC')
          if formeY==formeZ : 
             forme = '2'+formeY
          else : 
             forme = 'CS'   
          
          # creation des dictionnaires                   
          valdefac[name_AC_aster]  = l_fnor_mm
          valdirYac[name_AC_aster] = l_fy_mm
          valdirZac[name_AC_aster] = l_fz_mm
          valrho[name_AC_aster] = rho_mm
          valforme[name_AC_aster]= [formeY,formeZ,forme] 
          
          k=k+1

       for attr in POST_DEF:
          _unit       = attr['UNITE']
          _typ_post   = attr['FORMAT']

          DEFI_FICHIER(ACTION='LIBERER',UNITE=_unit)

          if (_typ_post=='GRACE'):

             _num_grille = attr['NUME_GRILLE']
             _extremum   = attr['TYPE_RESU']
             _autre      = attr['TYPE_VISU']

             if (_extremum == None):
                post = _num_grille
             else:
                post = _extremum

             if (_autre=='AMPLITUDE'):
                makeXMGRACEdef_amp(_unit,post,_coeur,valdefac)
             elif (_autre=='MODULE'):
                makeXMGRACEdef_mod(_unit,post,_coeur,valdefac)
             elif (_autre=='VECTEUR'):
                makeXMGRACEdef_vec(_unit,post,_coeur,valdefac,valdirYac,valdirZac)
             elif (_autre=='DEFORME'):
                name   = attr['POSITION']
                typeAC = attr['CONCEPTION']
                makeXMGRACEdeforme(_unit,name,typeAC,_coeur,valdefac)

          elif (_typ_post=='TABLE'):

             l_nom_AC=[]
             l_cycle = []
             l_repere = []
             l_def_max=[]
             l_XG1= []
             l_XG2= []
             l_XG3= []
             l_XG4= []
             l_XG5= []
             l_XG6= []
             l_XG7= []
             l_XG8= []
             l_XG9= []
             l_XG10= []
             l_YG1= []
             l_YG2= []
             l_YG3= []
             l_YG4= []
             l_YG5= []
             l_YG6= []
             l_YG7= []
             l_YG8= []
             l_YG9= []
             l_YG10= []
             l_milieu= []
             l_MinX= []
             l_MaxX= []
             l_CCX= []
             l_MinY= []
             l_MaxY= []
             l_CCY= []
             l_formeX= []
             l_formeY= []
             l_forme= []

             for name in POSITION :
                name_AC_aster = name[0]+'_'+name[1]
                name_AC_damac = _coeur.position_todamac(name_AC_aster)

                cycle=1
                repere='non_renseigne'
                def_max = valrho[name_AC_aster]
                XG1 =valdirYac[name_AC_aster][ 1-1]
                XG2 =valdirYac[name_AC_aster][ 2-1]
                XG3 =valdirYac[name_AC_aster][ 3-1]
                XG4 =valdirYac[name_AC_aster][ 4-1]
                XG5 =valdirYac[name_AC_aster][ 5-1]
                XG6 =valdirYac[name_AC_aster][ 6-1]
                XG7 =valdirYac[name_AC_aster][ 7-1]
                XG8 =valdirYac[name_AC_aster][ 8-1]
                XG9 =valdirYac[name_AC_aster][ 9-1]
                XG10=valdirYac[name_AC_aster][10-1]
                YG1 =valdirZac[name_AC_aster][ 1-1]
                YG2 =valdirZac[name_AC_aster][ 2-1]
                YG3 =valdirZac[name_AC_aster][ 3-1]
                YG4 =valdirZac[name_AC_aster][ 4-1]
                YG5 =valdirZac[name_AC_aster][ 5-1]
                YG6 =valdirZac[name_AC_aster][ 6-1]
                YG7 =valdirZac[name_AC_aster][ 7-1]
                YG8 =valdirZac[name_AC_aster][ 8-1]
                YG9 =valdirZac[name_AC_aster][ 9-1]
                YG10=valdirZac[name_AC_aster][10-1]
                Milieu = 'non_renseigne'
                MinX = min( valdirYac[name_AC_aster] )
                MaxX = max( valdirYac[name_AC_aster] )
                CCX  = MaxX - MinX
                MinY = min( valdirZac[name_AC_aster] )
                MaxY = max( valdirZac[name_AC_aster] )
                CCY  = MaxY - MinY
                FormeX = valforme[name_AC_aster][0]
                FormeY = valforme[name_AC_aster][1]
                Forme  = valforme[name_AC_aster][2]

                l_nom_AC.append(name_AC_damac)
                l_cycle.append(cycle)
                l_repere.append(repere)
                l_def_max.append(def_max)
                l_XG1.append(XG1) 
                l_XG2.append(XG2) 
                l_XG3.append(XG3) 
                l_XG4.append(XG4) 
                l_XG5.append(XG5) 
                l_XG6.append(XG6) 
                l_XG7.append(XG7) 
                l_XG8.append(XG8) 
                l_XG9.append(XG9) 
                l_XG10.append(XG10) 
                l_YG1.append(YG1) 
                l_YG2.append(YG2) 
                l_YG3.append(YG3) 
                l_YG4.append(YG4) 
                l_YG5.append(YG5) 
                l_YG6.append(YG6) 
                l_YG7.append(YG7) 
                l_YG8.append(YG8) 
                l_YG9.append(YG9) 
                l_YG10.append(YG10) 
                l_milieu.append(Milieu) 
                l_MinX.append(MinX) 
                l_MaxX.append(MaxX) 
                l_CCX.append(CCX ) 
                l_MinY.append(MinY) 
                l_MaxY.append(MaxY) 
                l_CCY.append(CCY)
                l_formeX.append(FormeX)
                l_formeY.append(FormeY)
                l_forme.append(Forme)

             # creation de la table de sortie
             _TABOUT = CREA_TABLE(LISTE=(_F(LISTE_K=l_nom_AC ,PARA='NOM_AC'),
                                         _F(LISTE_I=l_cycle,PARA='Cycle'),
                                         _F(LISTE_K=l_repere,PARA='Repere',TYPE_K='K16'),
                                         _F(LISTE_R=l_def_max,PARA='Ro'),
                                         _F(LISTE_R=l_XG1,PARA='XG1'),
                                         _F(LISTE_R=l_XG2,PARA='XG2'),
                                         _F(LISTE_R=l_XG3,PARA='XG3'),
                                         _F(LISTE_R=l_XG4,PARA='XG4'),
                                         _F(LISTE_R=l_XG5,PARA='XG5'),
                                         _F(LISTE_R=l_XG6,PARA='XG6'),
                                         _F(LISTE_R=l_XG7,PARA='XG7'),
                                         _F(LISTE_R=l_XG8,PARA='XG8'),
                                         _F(LISTE_R=l_XG9,PARA='XG9'),
                                         _F(LISTE_R=l_XG10,PARA='XG10'),
                                         _F(LISTE_R=l_YG1,PARA='YG1'),
                                         _F(LISTE_R=l_YG2,PARA='YG2'),
                                         _F(LISTE_R=l_YG3,PARA='YG3'),
                                         _F(LISTE_R=l_YG4,PARA='YG4'),
                                         _F(LISTE_R=l_YG5,PARA='YG5'),
                                         _F(LISTE_R=l_YG6,PARA='YG6'),
                                         _F(LISTE_R=l_YG7,PARA='YG7'),
                                         _F(LISTE_R=l_YG8,PARA='YG8'),
                                         _F(LISTE_R=l_YG9,PARA='YG9'),
                                         _F(LISTE_R=l_YG10,PARA='YG10'),
                                         _F(LISTE_K=l_milieu,PARA='Milieu',TYPE_K='K16'),
                                         _F(LISTE_R=l_MinX,PARA='Min_X'),
                                         _F(LISTE_R=l_MaxX,PARA='Max_X'),
                                         _F(LISTE_R=l_CCX,PARA='CC_X'),
                                         _F(LISTE_R=l_MinY,PARA='Min_Y'),
                                         _F(LISTE_R=l_MaxY,PARA='Max_Y'),
                                         _F(LISTE_R=l_CCY,PARA='CC_Y'),
                                         _F(LISTE_K=l_formeX ,PARA='Forme_X'),
                                         _F(LISTE_K=l_formeY ,PARA='Forme_Y'),
                                         _F(LISTE_K=l_forme  ,PARA='Forme'),
                                          )
                                  )

             # impression de la table de sortie
             IMPR_TABLE(TABLE=_TABOUT,
                        FORMAT_R='F5.1',
                        UNITE=_unit,
#                        SEPARATEUR='\t',
                        )
