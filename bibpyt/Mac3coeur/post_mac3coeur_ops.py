#@ MODIF post_mac3coeur_ops Mac3coeur  DATE 05/07/2011   AUTEUR FERNANDES R.FERNANDES 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
# RESPONSABLE FERNANDES R.FERNANDES
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
        xmgrfile.write('@with string\n@string on\n@string loctype world\n@string color (0,0,0)\n@string char size 0.8\n@string just 2\n@string %f, %f\n@string def \"%s\"\n' %(val_abs - (len(coeur.ALPHAMAC)-1)/2., (len(coeur.ALPHAMAC)-1)/2.+1.2, coeur.ALPHABET[val_abs]))
    for val_ord in range(0,len(coeur.NumV)):
        xmgrfile.write('@with string\n@string on\n@string loctype world\n@string color (0,0,0)\n@string char size 0.8\n@string just 2\n@string %f, %f\n@string def \"%s\"\n' %(-(len(coeur.ALPHAMAC)-1)/2.-1.5,  val_ord - (len(coeur.ALPHAMAC)-1)/2.-0.2, coeur.NumV[len(coeur.NumV)-1-val_ord]))
    xmgrfile.write('@kill s0\n@s0 line pattern 0\n@s0 symbol fill pattern 0\n%f %f\n%f %f\n' %(-(len(coeur.ALPHAMAC)-1)/2.-1.02, -(len(coeur.ALPHAMAC)-1)/2.-0.4,(len(coeur.ALPHAMAC)-1)/2.+0.5,(len(coeur.ALPHAMAC)-1)/2.+1.05))
    
    ind=0
    for k in POSITION:
       ind=ind+1
       y=coeur.ALPHAMAC.index(k[0])-(len(coeur.ALPHAMAC)-1)/2.
       x=coeur.ALPHAMAC.index(k[1])-(len(coeur.ALPHAMAC)-1)/2.
       xmgrfile.write('@kill s%d\n@s%d symbol 2\n@s%d symbol pattern 1\n@s%d symbol size 0.4\n@s%d symbol color 1\n@s%d symbol fill pattern 1\n@s%d symbol fill color 1\n@type xy\n%10.8f %10.8f\n' %(ind,ind,ind,ind,ind,ind,ind,x,y))

    for name in valjeuac.keys():
        position1 = name[4:6]
        position2 = name[6:8]
        (x1,y1) = NodePos(position1)
        (x2,y2) = NodePos(position2)
        if (post=='MAXI') :
            (redF,greenF,blueF,lame,size) = computeColor(max(valjeuac[name]))
            titre = 'maximaux'
        elif (post=='MINI') :
            (redF,greenF,blueF,lame,size) = computeColor(min(valjeuac[name]))
            titre = 'minimaux'
        else :
            (redF,greenF,blueF,lame,size) = computeColor(valjeuac[name][post-1])
            titre = 'au niveau des grilles %s'%post
        xmgrfile.write('@with string\n@string on\n@string loctype world\n@string color (%d,%d,%d)\n@string char size %f\n@string just 2\n@string %f, %f\n@string def \"%s\"\n' %(redF,greenF,blueF,size,(x1+x2)/2.0,(y1+y2)/2.0-0.1,lame))

    for name in valjeucu.keys():
        position1 = name[3:5]
        position2 = name[6:7]
        (x1,y1) = NodePos(position1)
        (x2,y2) = NodePosCu(position2)
        if (post=='MAXI') :
           (redF,greenF,blueF,lame,size) = computeColor(max(valjeucu[name]))
        elif (post=='MINI') :
           (redF,greenF,blueF,lame,size) = computeColor(min(valjeucu[name]))
        else :
           (redF,greenF,blueF,lame,size) = computeColor(valjeucu[name][post-1])
        xmgrfile.write('@with string\n@string on\n@string loctype world\n@string color (%d,%d,%d)\n@string char size %f\n@string just 2\n@string %f, %f\n@string def \"%s\"\n' %(redF,greenF,blueF,size,(x1+x2),(y1+y2)-0.1,lame))
    
    xmgrfile.write('&\n@xaxis ticklabel off\n@yaxis ticklabel off\n@xaxis tick off\n@yaxis tick off\n@subtitle \"Jeux %s entre les ACs du Coeur (en mm)"\n@DEVICE \"JPEG\" PAGE SIZE 1200,1200\n@autoscale\n@redraw\n'%(titre))
    xmgrfile.close()

def post_mac3coeur_ops(self, **args):
    """Corps principal de la macro de post-traitement de MAC3COEUR"""
    import aster
    from Accas import _F
    CREA_CHAMP    = self.get_cmd('CREA_CHAMP')
    CREA_TABLE    = self.get_cmd('CREA_TABLE')
    CALC_TABLE    = self.get_cmd('CALC_TABLE')
    EXTR_TABLE    = self.get_cmd('EXTR_TABLE')
    FORMULE       = self.get_cmd('FORMULE')
    DEFI_FICHIER  = self.get_cmd('DEFI_FICHIER')
    
    self.set_icmd(1)
    _RESU       = self['RESULTAT']
    _typ_coeur  = self['TYPE_COEUR']
    POST_LAME   = self['LAME']
    _inst       = self['INST']

    datg = self.jdc.args.get("rep_dex")
    coeur_factory = CoeurFactory(datg)
    _coeur = coeur_factory.get(_typ_coeur)('post', _typ_coeur, self, datg)

    if (POST_LAME != None) :
       CONT   = CREA_CHAMP(TYPE_CHAM='ELGA_VARI_R',OPERATION='EXTR',RESULTAT=_RESU,NOM_CHAM='VARI_ELGA',INST=_inst)

       valjeuac={}
       valjeucu={}

       _formule = FORMULE(NOM_PARA='V8',VALE='1000.*V8')
       print 'Recuperation des jeux inter-assemblages'
       for name in _coeur.nomContactAssLame:
          _TAB1 = CREA_TABLE(RESU=_F(CHAM_GD=CONT,NOM_CMP='V8',GROUP_MA=name))
          _TAB1 = CALC_TABLE(reuse=_TAB1,TABLE=_TAB1,
                            ACTION = (_F(OPERATION='FILTRE',NOM_PARA='POINT',CRIT_COMP='EQ',VALE_I=1),
                                      _F(OPERATION='TRI',NOM_PARA='COOR_X',ORDRE='CROISSANT'),
                                      _F(OPERATION='OPER',FORMULE=_formule,NOM_PARA='P_LAME')))
          tab1 = _TAB1.EXTR_TABLE()
          valjeuac[name] = tab1.P_LAME.values()
     
       print 'Recuperation des jeux entre les assemblages de bord et le cloisonnement'
       for name in _coeur.nomContactCuve:
          _TAB1 = CREA_TABLE(RESU=_F(CHAM_GD=CONT,NOM_CMP='V8',GROUP_MA=name))
          _TAB1 = CALC_TABLE(reuse=_TAB1,TABLE=_TAB1,
                            ACTION = (_F(OPERATION='FILTRE',NOM_PARA='POINT',CRIT_COMP='EQ',VALE_I=1),
                                      _F(OPERATION='TRI',NOM_PARA='COOR_X',ORDRE='CROISSANT'),
                                      _F(OPERATION='OPER',FORMULE=_formule,NOM_PARA='P_LAME')))
          tab1 = _TAB1.EXTR_TABLE()
          valjeucu[name] = tab1.P_LAME.values()

       for attr in POST_LAME:
          _num_grille = attr['NUME_GRILLE']
          _unit       = attr['UNITE']
          _extremum   = attr['TYPE_RESU']
          
          DEFI_FICHIER(ACTION='LIBERER',UNITE=_unit)
          
          if (_extremum == None):
             print 'Realisation de la sortie de post-traitement sur la grille ',_num_grille
             post = _num_grille
          else:
             print 'Realisation de la sortie de post-traitement sur la valeur ',_extremum
             post = _extremum
          
          makeXMGRACEjeu(_unit,post,_coeur,valjeuac,valjeucu)
       
