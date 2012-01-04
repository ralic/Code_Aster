#@ MODIF mac3coeur_coeur Mac3coeur  DATE 04/01/2012   AUTEUR SELLENET N.SELLENET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

"""
Module dédié à la macro MAC3COEUR.

Définition d'une conception de coeur (ensemble d'assemblages).
"""

import os
import string

from mac3coeur_factory import Mac3Factory
from mac3coeur_assemblage import ACFactory


class Coeur(object):
    """Classe définissant un coeur de reacteur."""
    type_coeur = None
    required_parameters = [
        #Nombre d'assemblages pour définir le coeur
        'NBAC',
        # Position des grilles pour definition du champ de fluence
        'alt_g1', 'alt_g2', 'alt_gm', 'alt_gn', 'altitude',
        # Position des crayons et tubes-guides pour definition du champ de fluence
        'XINFT', 'XSUPT', 'XINFC', 'XSUPC', 'LONCR',
        # Caractéristique de la cuve
        'pas_assemblage',
        # Geometrie du coeur
        'ALPHABET','ALPHAMAC','NumV',
        # Post-traitement des lames
        'nomContactAssLame','nomContactCuve',
    ]
    _time        = ('T0', 'T1', 'T2', 'T3', 'T4', 'T5', 'T6', 'T7', 'T8', 'T9',)
    _subtime     = ('N0', 'N1', 'N2', 'N3', 'N4', 'N5', 'N6', 'N7', 'N8', 'N9')

    def __init__(self, name, typ_coeur, macro, datg):
        """Initialisation d'un type de coeur."""
        self.name           = name
        self.macro          = macro
        self.typ_coeur      = typ_coeur
        self.nbac           = 0
        self.factory        = ACFactory(datg)
        self.collAC         = {}
        self._mateAC        = {}
        self.nameAC         = {}
        self.temps_simu     = {}.fromkeys(self._time)
        self.sub_temps_simu = {}.fromkeys(self._subtime)
        self._para          = {}
        self._keys          = {}.fromkeys(self.required_parameters)
        self._init_from_attrs()
        self.dcorr1 = dict(zip(self.ALPHABET, self.ALPHAMAC))
        self.dcorr2 = dict(zip(self.ALPHAMAC, self.ALPHABET))
        self.dnume2 = dict(zip(self.ALPHAMAC, range(1, len(self.ALPHAMAC) + 1)))

    def _init_from_attrs(self):
        """Initialisation à partir des attributs de classe."""
        for attr in dir(self):
            if self._keys.get(attr):
                self._para[attr] = getattr(self, attr)

    def __getattr__(self, para):
        """Retourne la valeur d'un paramètre."""
        if self._para.get(para) is None:
            raise KeyError("parameter not defined : '%s'" % para)
        return self._para.get(para)

    def get_geom_coeur(self):
        """Retourne la géométrie du coeur."""
        raise NotImplementedError

    def position_toaster(self, position):
        """Retourne la position Aster correspondant à la position DAMAC."""
        lig, col = position[0], position[1:]
        ind = int(col) - 1
        try:
          posi_aster = self.ALPHAMAC[ind] + "_" + self.dcorr1[lig]
        except (IndexError, KeyError):
            raise KeyError("invalid damac position : %s" % position)
        return posi_aster

    def position_todamac(self,position):
        """Retourne la position DAMAC correspondant à la position Aster."""
        col, lig = position.split("_")
        try:
           posi_damac = self.dcorr2[lig] + '%02d' %(self.dnume2[col])
        except KeyError:
            raise KeyError("invalid aster position : %s" % position)

        return posi_damac

    def position_fromthyc(self, posX, posY):
        """Retourne la position Aster correspondant à la position Thyc."""
        lig, col = position[0], position[1:]
        ind = int(col) - 1
        try:
          posi_aster = self.ALPHAMAC[ind] + "_" + self.dcorr1[lig]
        except (IndexError, KeyError):
            raise KeyError("invalid damac position : %s" % position)
        return posi_aster

    def init_from_table(self, tab):
        """Initialise le coeur à partir d'une table."""
        self.nbac = len(tab)
        for rows in tab:
            idAC   = rows['idAC'].strip()
            typeAC = rows['Milieu'].strip()
            nameAC = rows['Repere'].strip()
            ac = self.factory.get(typeAC)(self.typ_coeur)
            ac.register_position(self.position_toaster, self.position_todamac)
            ac.place(idAC, rows['Cycle'])
            if self._mateAC.get(typeAC) is None:
                self._mateAC[typeAC] = MateriauAC(typeAC, self.macro)
            ac_def = {}
            for igr in range(0, ac._para['NBGR']):
               ac_def['DY'+str(igr+1)] =   rows['XG'+str(igr+1)]/1000.0
               ac_def['DZ'+str(igr+1)] = - rows['YG'+str(igr+1)]/1000.0
            ac.set_deforDAM(ac_def)
            ac.set_materiau(self._mateAC[typeAC])
            ac.check()
            self.collAC[idAC]   = ac
            self.nameAC[nameAC] = ac.idAST

    def cherche_rubrique_nom(self,f, nom):
        "Chercher une rubrique definie par son nom"
        while 1:
            line = f.readline()
            if (not line):
                break
            strtmp = line[0:len(line)-1]
            if (len(line) >= len(nom)):
                if (line[0:len(nom)] == nom):
                    return 1
        return None

    def definir_chargement_transverse(self,cote,epaisseur,pos_thyc,force,prod):
        DEFI_FONCTION    = self.macro.get_cmd('DEFI_FONCTION')
        "Determination du chargement transverse sur les crayons pour un assemblage donne."
        kk=2
        eps = 1.0e-6

        defi_fonc = []
        "Pour aller de l embout inferieur jusqu'a la premiere grille."
        som_l = 0.0
        som_f = 0.0
        for k in range(kk,pos_thyc[0]):
            som_l = som_l + string.atof(epaisseur[k])
            som_f = som_f + prod*string.atof(force[k])/string.atof(epaisseur[k])
        som_feq = som_l/(som_l + 0.5*string.atof(epaisseur[pos_thyc[0]]))*som_f
        defi_fonc.append(string.atof(cote[kk])-0.5*string.atof(epaisseur[kk])-eps)
        defi_fonc.append(som_feq)
        defi_fonc.append(string.atof(cote[pos_thyc[0]])-0.5*string.atof(epaisseur[pos_thyc[0]])+eps)
        defi_fonc.append(som_feq)
        defi_fonc.append(string.atof(cote[pos_thyc[0]]))
        defi_fonc.append(0.0)
        
        "Pour aller de la premiere a la derniere grille."
        for j in range(0,len(pos_thyc)-1):
            som_l = 0.0
            som_f = 0.0
            for k in range(pos_thyc[j]+1,pos_thyc[j+1]):
                som_l = som_l + string.atof(epaisseur[k])
                som_f = som_f + prod*string.atof(force[k])/string.atof(epaisseur[k])
            som_feq = som_l/(som_l + 0.5*(string.atof(epaisseur[pos_thyc[j]])+string.atof(epaisseur[pos_thyc[j+1]])))*som_f
            defi_fonc.append(string.atof(cote[pos_thyc[j]])+0.5*string.atof(epaisseur[pos_thyc[j]])-eps)
            defi_fonc.append(som_feq)
            defi_fonc.append(string.atof(cote[pos_thyc[j+1]])-0.5*string.atof(epaisseur[pos_thyc[j+1]])+eps)
            defi_fonc.append(som_feq)
            defi_fonc.append(string.atof(cote[pos_thyc[j+1]]))
            defi_fonc.append(0.0)

        "Pour aller de la derniere grille jusqu'a l embout superieur."
        som_l = 0.0
        som_f = 0.0
        for k in range(pos_thyc[len(pos_thyc)-1]+1,len(cote)):
            som_l = som_l + string.atof(epaisseur[k])
            som_f = som_f + prod*string.atof(force[k])/string.atof(epaisseur[k])
        som_feq = som_l/(som_l + 0.5*string.atof(epaisseur[len(cote)-1]))*som_f
        defi_fonc.append(string.atof(cote[pos_thyc[len(pos_thyc)-1]])+0.5*string.atof(epaisseur[pos_thyc[len(pos_thyc)-1]])-eps)
        defi_fonc.append(som_feq)
        defi_fonc.append(string.atof(cote[len(cote)-1])+0.5*string.atof(epaisseur[len(cote)-1])+eps)
        defi_fonc.append(som_feq)

        _resu = DEFI_FONCTION(NOM_PARA='X',VALE=defi_fonc)
        return _resu

    def lire_resu_thyc(self,MODELE,nom_fic):
        DEFI_FONCTION    = self.macro.get_cmd('DEFI_FONCTION')
        AFFE_CHAR_MECA   = self.macro.get_cmd('AFFE_CHAR_MECA')
        AFFE_CHAR_MECA_F = self.macro.get_cmd('AFFE_CHAR_MECA_F')
        """ Fonction multiplicative de la force hydrodynamique axiale.
            On multiplie par 0.722 les forces hydrodynamiques a froid pour obtenir celles a chaud."""
        FOHYFR_1 = 1.0    # Valeur a froid
        FOHYCH_1 = 0.722  # Valeur a chaud

        from Accas import _F
        f  = open(nom_fic, 'r')
        f2 = open(nom_fic, 'r')
        self.cherche_rubrique_nom(f, ' EFFORTS TRANSVERSES selon X en N')
        self.cherche_rubrique_nom(f2, ' EFFORTS TRANSVERSES selon Y en N')
        line = f.readline().split()
        line2 = f2.readline().split()
        line2 = f2.readline().split()

        # Recuperation de l'epaisseur des mailles dans Thyc
        epaisseur = f.readline().split()
        if (epaisseur[0]!="ep(m)"):
            raise KeyError("invalid epaisseur")

        cote = f.readline().split()
        if (cote[0]!="Z(m)"):
            raise KeyError("invalid cote axial")

        j = 0
        pos_thyc=[]
        for i in range(2,len(cote)):
            #Positionnement des grilles
            if ((self.altitude[j]>(string.atof(cote[i])-string.atof(epaisseur[i])/2.)) & (self.altitude[j]<(string.atof(cote[i])+string.atof(epaisseur[i])/2.))):
                pos_thyc.append(i)
                j=j+1
                if (j==len(self.altitude)):
                    break

        for i in range(2,len(cote)):
            #Positionnement des crayons pour application des efforts transverses
            if ((self.XINFC>(string.atof(cote[i])-string.atof(epaisseur[i])/2.)) & (self.XINFC<(string.atof(cote[i])+string.atof(epaisseur[i])/2.))):
                pos_gril_inf = i
            if ((self.XSUPC>(string.atof(cote[i])-string.atof(epaisseur[i])/2.)) & (self.XSUPC<(string.atof(cote[i])+string.atof(epaisseur[i])/2.))):
                pos_gril_sup = i
    
        # Recuperation des efforts transverses sur les grilles
        mcf = []
        mcft= []
        for i in range(0,self.NBAC):
            line  = f.readline().split()
            line2 = f2.readline().split()
            posi_aster1 = self.ALPHAMAC[len(self.ALPHAMAC)+2-string.atoi(line[1])-1]  + "_" + self.ALPHAMAC[string.atoi(line[0])-2]
            posi_aster2 = self.ALPHAMAC[len(self.ALPHAMAC)+2-string.atoi(line2[1])-1] + "_" + self.ALPHAMAC[string.atoi(line2[0])-2]
            
            print line[0],line[1],posi_aster1
            if (posi_aster1!=posi_aster2):
                raise KeyError("position d assemblage avec ordre different")
    
            for j in range(0,len(pos_thyc)):
               mtmp = (_F(GROUP_NO = 'G_'+posi_aster1+'_'+str(j+1), FY = string.atof(line[pos_thyc[j]])/4.0, FZ = - string.atof(line2[pos_thyc[j]])/4.0),)
               mcf.extend(mtmp)
        
            _resu_fy = self.definir_chargement_transverse(cote,epaisseur,pos_thyc,line,1)
            _resu_fz = self.definir_chargement_transverse(cote,epaisseur,pos_thyc,line2,-1)
            mtmp = (_F(GROUP_MA = 'CR_'+posi_aster1, FY = _resu_fy, FZ = _resu_fz),)
            mcft.extend(mtmp)
        
        _AF_CHTRNO = AFFE_CHAR_MECA(MODELE=MODELE,FORCE_NODALE = mcf)
        _AF_CHTRFX = AFFE_CHAR_MECA_F(MODELE=MODELE,FORCE_POUTRE = mcft)

        # Recuperation des efforts axiaux
        self.cherche_rubrique_nom(f, ' *    FORCE HYDRODYNAMIQUE AXIALE en (N)           *')
        line = f.readline().split()
        line = f.readline().split()
        line = f.readline().split()
        
        mcp  = []
        mcpf = []
        for i in range(0,self.NBAC):
            line = f.readline().split()
            posi_aster = self.ALPHAMAC[len(self.ALPHAMAC)+2-string.atoi(line[1])-1]+ "_" +self.ALPHAMAC[len(self.ALPHAMAC)+2-string.atoi(line[0])-1]
            idAC=self.position_todamac(posi_aster)

            ac=self.collAC[idAC]
            KTOT = ac.K_GRM*(ac.NBGR-2)+ac.K_GRE*2+ac.K_EBSU+ac.K_TUB+ac.K_EBIN

            # Force axiale pour une grille extremite (inf)
            mtmp = (_F(GROUP_NO = 'G_'+posi_aster+'_'+str(1), FX = string.atof(line[2])/FOHYCH_1*ac.K_GRE/KTOT/4.0),)
            mcp.extend(mtmp)

            # Force axiale pour chacune des grilles de mélange
            for j in range(1,ac.NBGR-1):
                mtmp = (_F(GROUP_NO = 'G_'+posi_aster+'_'+str(j+1), FX = string.atof(line[2])/FOHYCH_1*ac.K_GRM/KTOT/4.0),)
                mcp.extend(mtmp)

            # Force axiale pour une grille extremite (sup)
            mtmp = (_F(GROUP_NO = 'G_'+posi_aster+'_'+str(ac.NBGR), FX = string.atof(line[2])/FOHYCH_1*ac.K_GRE/KTOT/4.0),)
            mcp.extend(mtmp)

            # Force axiale pour l'embout inferieur
            mtmp = (_F(GROUP_NO = 'PI_'+posi_aster, FX = string.atof(line[2])/FOHYCH_1*ac.K_EBIN/KTOT),)
            mcp.extend(mtmp)
            
            # Force axiale pour l'embout superieur
            mtmp = (_F(GROUP_NO = 'PS_'+posi_aster, FX = string.atof(line[2])/FOHYCH_1*ac.K_EBSU/KTOT),)
            mcp.extend(mtmp)
            
            # Force axiale pour les crayons (appel a DEFI_FONCTION)
            vale = string.atof(line[2])/FOHYCH_1*ac.K_TUB/KTOT*ac.NBCR/(ac.NBCR+ac.NBTG)/ac.LONCR
            _FXC = DEFI_FONCTION(NOM_PARA='X',VALE=(ac.XINFC,vale,ac.XSUPC,vale))            
            mtmp = (_F(GROUP_MA = 'CR_'+posi_aster, FX = _FXC),)
            mcpf.extend(mtmp)
            
            # Force axiale pour les tubes-guides (appel a DEFI_FONCTION)
            vale = string.atof(line[2])/FOHYCH_1*ac.K_TUB/KTOT*ac.NBTG/(ac.NBCR+ac.NBTG)/ac.LONTU
            _FXT = DEFI_FONCTION(NOM_PARA='X',VALE=(ac.XINFT,vale,ac.XSUPT,vale))            
            mtmp = (_F(GROUP_MA = 'TG_'+posi_aster, FX = _FXT),)
            mcpf.extend(mtmp)

        _AF_CHAXNO = AFFE_CHAR_MECA(MODELE=MODELE,FORCE_NODALE = mcp)
        _AF_CHAXPO = AFFE_CHAR_MECA_F(MODELE=MODELE,FORCE_POUTRE = mcpf)

        return _AF_CHTRNO,_AF_CHTRFX,_AF_CHAXNO,_AF_CHAXPO

    def chargement_defor(self):
        """Retourne les deformations de la TABLE."""
        from Accas import _F
        mcf = []
        for ac in self.collAC.values():
            mcf.extend(ac.chargement_defor())

        mtmp = (_F(GROUP_MA = 'CRAYON', DRX = 0.0,),
                _F(GROUP_NO = 'LISPG',  DRX = 0.0, DRY = 0.0, DRZ = 0.0,),
                _F(GROUP_MA =('EBOSUP','EBOINF'),  DX  = 0.0, DY  = 0.0, DZ = 0.0, DRX=0.0, DRY=0.0, DRZ=0.0,),
                _F(GROUP_NO = 'P_CUV',  DX  = 0.0, DY  = 0.0, DZ  = 0.0,))
        mcf.extend(mtmp)

        return mcf

    def repr(self):
        """Liste les assemblages."""
        txt = ["Lecture du Coeur %s - composé de %d assemblages" \
               % (self.name, self.nbac)]
        all = self.collAC.items()
        all.sort()
        txt.append("position_DAMAC correspondance_Code_Aster Type_de_conception Nombre_de_cycle" )
        for idAC, ac in all:
            txt.append("%8s %8s %8s %i" % (idAC, ac.idAST, ac.typeAC, ac._cycle))
        return os.linesep.join(txt)

    def mcf_geom_fibre(self):
        """Retourne les mots-clés facteurs pour DEFI_GEOM_FIBRE."""
        mcf = []
        for ac in self.collAC.values():
            mcf.extend(ac.mcf_geom_fibre())
        return mcf

    def mcf_cara_multifibre(self):
        """Retourne les mots-clés facteurs pour AFFE_CARA_ELEM/MULTIFIBRE."""
        mcf = []
        for ac in self.collAC.values():
            mcf.extend(ac.mcf_cara_multifibre())
        return mcf

    def mcf_cara_poutre(self):
        """Retourne les mots-clés facteurs pour AFFE_CARA_ELEM/POUTRE."""
        mcf = []
        for ac in self.collAC.values():
            mcf.extend(ac.mcf_cara_poutre())
        return mcf

    def mcf_cara_discret(self):
        """Retourne les mots-clés facteurs pour AFFE_CARA_ELEM/DISCRET."""
        mcf = []
        for ac in self.collAC.values():
            mcf.extend(ac.mcf_cara_discret())
        return mcf

    def definition_geom_fibre(self):
        DEFI_GEOM_FIBRE = self.macro.get_cmd('DEFI_GEOM_FIBRE')

        mcf = self.mcf_geom_fibre()
        _GFF=DEFI_GEOM_FIBRE(FIBRE=mcf,);

        return _GFF

    def affe_char_lame(self,MODELE):
        AFFE_CHAR_CINE = self.macro.get_cmd('AFFE_CHAR_CINE')
        mcf = self.chargement_defor()

        _AF_CIN = AFFE_CHAR_CINE(MODELE=MODELE,MECA_IMPO = mcf)
        return _AF_CIN

    def chargement_archimede1(self):
        """Retourne les mots-clés facteurs pour AFFE_CHAR_MECA/FORCE_NODALE."""
        mcf = []
        for ac in self.collAC.values():
            mcf.extend(ac.chargement_archimede1())
        return mcf

    def definition_archimede1(self,MODELE):
        AFFE_CHAR_MECA = self.macro.get_cmd('AFFE_CHAR_MECA')
        mcf = self.chargement_archimede1()

        _ARCH_1 = AFFE_CHAR_MECA(MODELE=MODELE,FORCE_NODALE = mcf)
        return _ARCH_1

    def chargement_archimede2(self):
        """Retourne les mots-clés facteurs pour AFFE_CHAR_MECA_F/FORCE_POUTRE."""
        DEFI_FONCTION = self.macro.get_cmd('DEFI_FONCTION')
        mcf = []
        for ac in self.collAC.values():
            _FCT_TG = DEFI_FONCTION(NOM_PARA = 'X',PROL_DROITE = 'CONSTANT',PROL_GAUCHE = 'CONSTANT',
                                    VALE     = (ac.XINFT,(ac.AFTG_1 / ac.LONTU ),
                                                     ac.XSUPT,(ac.AFTG_1 / ac.LONTU )))
            _FCT_CR = DEFI_FONCTION(NOM_PARA = 'X',PROL_DROITE = 'CONSTANT',PROL_GAUCHE = 'CONSTANT',
                                    VALE     = (ac.XINFC,(ac.AFCRA_1 / ac.LONCR ),
                                                     ac.XSUPC,(ac.AFCRA_1 / ac.LONCR )))
            mcf.extend(ac.chargement_archimede2(_FCT_TG,_FCT_CR))
        return mcf
                
    def definition_archimede2(self,MODELE):
        AFFE_CHAR_MECA_F = self.macro.get_cmd('AFFE_CHAR_MECA_F')
        mcf = self.chargement_archimede2()

        _FOARCH_1 = AFFE_CHAR_MECA_F(MODELE=MODELE,FORCE_POUTRE = mcf)
        return _FOARCH_1

    def definition_temp_archimede(self):
        DEFI_FONCTION = self.macro.get_cmd('DEFI_FONCTION')
        """ Valeur  a froid (20 degres) de la force d'Archimede = 860/985.46*1000.52 """
        ARCHFR1 = 873.  # Valeur en arret a froid (20 degres)
        ARCHFR2 = 860.  # Valeur en arret a froid (60 degres)
        ARCHCH  = 620.  # Valeur a chaud (307 degres)   

        _ARCH_F1 = DEFI_FONCTION( NOM_PARA = 'INST',PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT',
                                  VALE     = ( self.temps_simu['T0'],ARCHFR1,
                                               self.temps_simu['T1'],ARCHFR1,
                                               self.temps_simu['T2'],ARCHFR2,
                                               self.temps_simu['T4'],ARCHCH, 
                                               self.temps_simu['T5'],ARCHCH, 
                                               self.temps_simu['T7'],ARCHFR2,
                                               self.temps_simu['T8'],ARCHFR1,
                                               self.temps_simu['T9'],ARCHFR1,),);
        return _ARCH_F1

    def definition_temp_hydro_axiale(self):
        DEFI_FONCTION = self.macro.get_cmd('DEFI_FONCTION')
        """ Fonction multiplicative de la force hydrodynamique axiale.
            On multiplie par 0.722 les forces hydrodynamiques a froid pour obtenir celles a chaud."""
        FOHYFR_1 = 1.0    # Valeur a froid
        FOHYCH_1 = 0.722  # Valeur a chaud

        _HYDR_F1 = DEFI_FONCTION( NOM_PARA = 'INST',PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT',
                                  VALE     = ( self.temps_simu['T0'],0.0,
                                               self.temps_simu['T1'],0.0,
                                               self.temps_simu['T2'],FOHYFR_1,
                                               self.temps_simu['T3'],FOHYCH_1,
                                               self.temps_simu['T4'],FOHYCH_1, 
                                               self.temps_simu['T5'],FOHYCH_1, 
                                               self.temps_simu['T6'],FOHYCH_1, 
                                               self.temps_simu['T7'],FOHYFR_1,
                                               self.temps_simu['T8'],0.0,
                                               self.temps_simu['T9'],0.0,),);
        return _HYDR_F1

    def definition_effort_transverse(self):
        DEFI_FONCTION = self.macro.get_cmd('DEFI_FONCTION')
        """ Fonction multiplicative pour la prise en compte des efforts transverses."""
        AVEC = 1.0
        SANS = 0.0

        _F_TRAN1 = DEFI_FONCTION( NOM_PARA = 'INST',PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT',
                                  VALE     = ( self.temps_simu['T0'],SANS,
                                               self.temps_simu['T1'],SANS,
                                               self.temps_simu['T2'],SANS,
                                               self.temps_simu['T4'],AVEC, 
                                               self.temps_simu['T5'],AVEC, 
                                               self.temps_simu['T7'],SANS,
                                               self.temps_simu['T8'],SANS,
                                               self.temps_simu['T9'],SANS,),);
        return _F_TRAN1

    def definition_cara_coeur(self,MODELE,_GFF):
        from Accas import _F
        AFFE_CARA_ELEM = self.macro.get_cmd('AFFE_CARA_ELEM')

        mcm = self.mcf_cara_multifibre()
        mcp = self.mcf_cara_poutre()
        mtmp=_F(GROUP_MA='DIL',SECTION='RECTANGLE',CARA=('HY','HZ'),VALE=(0.03,0.21338),)
        mcp.append(mtmp)
        mcd = self.mcf_cara_discret()
        mtmp=_F(GROUP_MA=('RES_EXT','RES_CONT',),REPERE='LOCAL',CARA='K_T_D_L',VALE=(0.,0.,0.,),)
        mcd.append(mtmp)

        print 'debut de la procedure AFFE_CARA_ELEM'
        _CARA = AFFE_CARA_ELEM( MODELE      = MODELE,
                                POUTRE      = mcp,
                                GEOM_FIBRE  = _GFF,
                                MULTIFIBRE  = mcm,
                                DISCRET     = mcd,
                    ORIENTATION = (_F(GROUP_MA=('ELA_EX','ELA_ME','RIG_EX','RIG_ME','DIL'),CARA='VECT_Y',VALE=(1.,0.,0.),),),)
        print 'fin de la procedure AFFE_CARA_ELEM'
        return _CARA

    def definition_pesanteur(self,MODELE):
        from Accas import _F
        AFFE_CHAR_MECA = self.macro.get_cmd('AFFE_CHAR_MECA')

        _PESA = AFFE_CHAR_MECA( MODELE      =    MODELE,
                                PESANTEUR   = _F(GRAVITE=9.81,DIRECTION=(-1.,0.,0.),),)
        return _PESA

    def definition_effor_maintien(self):
        DEFI_FONCTION = self.macro.get_cmd('DEFI_FONCTION')

        _EFF_SM1 = 6300.+110;
        #_EFF_SM1 = (6300.+110)*1.1;

        _F_EBS = DEFI_FONCTION( NOM_PARA = 'INST',
                                VALE     = ( self.temps_simu['T0'],0.0,
                                             self.temps_simu['T1'],1.0*_EFF_SM1,
                                             self.temps_simu['T2'],1.0*_EFF_SM1,
                                             self.temps_simu['T4'],1.0*_EFF_SM1,
                                             self.temps_simu['T5'],1.0*_EFF_SM1,
                                             self.temps_simu['T7'],1.0*_EFF_SM1,
                                             self.temps_simu['T8'],1.0*_EFF_SM1,
                                             self.temps_simu['T9'],0.0,),
                                PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',);
        return _F_EBS

    def affectation_maillage(self,MA0):
        from Accas import _F
        CREA_MAILLAGE = self.macro.get_cmd('CREA_MAILLAGE')
        DEFI_GROUP    = self.macro.get_cmd('DEFI_GROUP')

        #__unit=DEFI_FICHIER(ACTION='RESERVER')

        #EXEC_LOGICIEL(LOGICIEL='/opt/salome/Salome-V5_1_5_64bit/runSalome',
        #           ARGUMENT='-t < /home/fernandes/CAC/Raphael/CHOOZ_C00_Version1/Version_finale_21_07_10_MODIF_DILA__DIS_CHOC/maquette_maillage_coeur_21_07_2010_DILA_POU__DIS_CHOC.py',);

        #MA0 = LIRE_MAILLAGE(FORMAT='MED',UNITE=__unit)

        LISGRIL =[]
        LISGRILI=[]
        LISGRILE=[]
        LISG    =[]
        LIS_PG  =[]
        nbgrmax = 0
        for ac in self.collAC.values():
           nbgrmax = max(nbgrmax,ac._para['NBGR'])
           LIS_GNO=[]
           for igr in range(0,ac._para['NBGR']):
              LIS_GNO.append('G_'+ac.idAST+'_'+str(igr+1))
              LIS_PG.append('P_'+ac.idAST+'_'+str(igr+1))

           DICG={}
           DICG["GROUP_NO"]=tuple(LIS_GNO)
           DICG["NOM_GROUP_MA"]='GR_'+ac.idAST
           LISG.append(DICG)

        for igr in range(0,nbgrmax):
            DICGRIL={}
            DICGRIL["GROUP_NO"]      = 'GRIL_'+str(igr+1)
            DICGRIL["NOM_GROUP_MA"]  = 'GRIL_'+str(igr+1)
            LISGRIL.append(DICGRIL)

            if igr==0:
               LISGRILE.append('GRIL_'+str(igr+1))
            elif igr==(nbgrmax-1):
               LISGRILE.append('GRIL_'+str(igr+1))
            else:
               LISGRILI.append('GRIL_'+str(igr+1))

        print 'Adaptation du maillage'
        _MA=CREA_MAILLAGE(MAILLAGE=MA0,
                      CREA_POI1=tuple(LISGRIL+LISG),)

        print 'Defi_group sur le maillage'
        _MA=DEFI_GROUP( reuse         = _MA, ALARME='NON',
                MAILLAGE = _MA,
                CREA_GROUP_MA=(_F(NOM='GRIL_I',UNION=tuple(LISGRILI),),
                               _F(NOM='GRIL_E',UNION=tuple(LISGRILE),),),
                CREA_GROUP_NO=(_F(GROUP_MA=('T_GUIDE','EBOSUP','EBOINF','CRAYON','ELA','DIL',),),
                               _F(NOM='LISPG',UNION =tuple(LIS_PG),),),);

        return _MA

    def cl_rigidite_grille(self):
        from Accas import _F

        mcf = []
        for ac in self.collAC.values():
           LIS_GNO=[]
           for igr in range(0,ac._para['NBGR']):
              mcf.append(_F(GROUP_NO='G_'+ac.idAST+'_'+str(igr+1)))
        return mcf

    def affectation_modele(self,MAILLAGE):
        from Accas import _F
        AFFE_MODELE = self.macro.get_cmd('AFFE_MODELE')
        _MODELE   = AFFE_MODELE(MAILLAGE =  MAILLAGE,
                   AFFE = (_F( GROUP_MA     = 'CRAYON',
                           PHENOMENE    = 'MECANIQUE',
                           MODELISATION = 'POU_D_TGM',),
                       _F( GROUP_MA     = 'T_GUIDE',
                           PHENOMENE    = 'MECANIQUE',
                           MODELISATION = 'POU_D_TGM',),
                       _F( GROUP_MA     =('EBOSUP','EBOINF'),
                           PHENOMENE    = 'MECANIQUE',
                           MODELISATION = 'POU_D_T',),
                       _F( GROUP_MA     =('ELA','RIG'),
                           PHENOMENE    = 'MECANIQUE',
                           MODELISATION = 'DIS_TR',),
                       _F( GROUP_MA     = 'DIL',
                           PHENOMENE    = 'MECANIQUE',
                           MODELISATION = 'POU_D_E',),
                       _F( GROUP_MA     =('GRIL_I','GRIL_E',),
                           PHENOMENE    = 'MECANIQUE',
                           MODELISATION = 'DIS_T',),
                       _F( GROUP_MA     =('RES_EXT','RES_CONT'),
                           PHENOMENE    = 'MECANIQUE',
                           MODELISATION = 'DIS_T',),),);

        return _MODELE

    def definition_time(self,fluence):
        from Accas import _F
        DEFI_LIST_REEL = self.macro.get_cmd('DEFI_LIST_REEL')
        DEFI_LIST_INST = self.macro.get_cmd('DEFI_LIST_INST')

        def m_time(a):
            m_time = ( _F( JUSQU_A = self.temps_simu[self._time[a]], NOMBRE = self.sub_temps_simu[self._subtime[a]],),)
            return m_time

        self.init_temps_simu(fluence)

        _list = []
        for _time in range(1,len(self._time)):
            _list.extend(m_time(_time))

        _LI=DEFI_LIST_REEL(DEBUT=self.temps_simu['T0'],INTERVALLE = _list,);

        _TE = DEFI_LIST_INST(DEFI_LIST = _F(LIST_INST = _LI,),
                             ECHEC     = _F(SUBD_PAS  = 2, SUBD_NIVEAU = 5,),)

        return _TE

    def init_temps_simu(self,fluence):
        """Initialise les temps caracteristiques"""
        Dt = 1.e-3
        self.temps_simu['T0'] = 0.0
        self.temps_simu['T1'] = self.temps_simu['T0'] + Dt ;
        self.temps_simu['T2'] = self.temps_simu['T1'] + Dt ;
        self.temps_simu['T3'] = self.temps_simu['T2'] + Dt ;
        self.temps_simu['T4'] = self.temps_simu['T3'] + Dt ;
        self.temps_simu['T5'] = self.temps_simu['T4'] + max(fluence,Dt);
        self.temps_simu['T6'] = self.temps_simu['T5'] + Dt ;
        self.temps_simu['T7'] = self.temps_simu['T6'] + Dt ;
        self.temps_simu['T8'] = self.temps_simu['T7'] + Dt ;
        self.temps_simu['T9'] = self.temps_simu['T8'] + Dt ;

        self.sub_temps_simu['N0'] =  2;
        self.sub_temps_simu['N1'] =  2;
        self.sub_temps_simu['N2'] =  2;
        self.sub_temps_simu['N3'] =  2;
        self.sub_temps_simu['N4'] =  2;
        self.sub_temps_simu['N5'] = 50;
        self.sub_temps_simu['N6'] =  2;
        self.sub_temps_simu['N7'] =  2;
        self.sub_temps_simu['N8'] =  2;
        self.sub_temps_simu['N9'] =  2;

    def definition_fluence(self,fluence,MAILLAGE):
        from Accas import _F
        CREA_CHAMP    = self.macro.get_cmd('CREA_CHAMP')
        DEFI_FONCTION = self.macro.get_cmd('DEFI_FONCTION')
        DEFI_NAPPE    = self.macro.get_cmd('DEFI_NAPPE')
        FORMULE       = self.macro.get_cmd('FORMULE')
        CREA_RESU     = self.macro.get_cmd('CREA_RESU')

        ##############################################################
        #   CREATION D UNE NAPPE DE FLUX NEUTRONIQUE DANS LE COEUR   #
        ##############################################################
        #   CREATION DE LA PARTIE GEOMETRIQUE        #
        #########################################
        _CHXN = CREA_CHAMP( OPERATION = 'EXTR', TYPE_CHAM = 'NOEU_GEOM_R', NOM_CHAM  = 'GEOMETRIE', MAILLAGE  =  MAILLAGE);

        ########################################
        #   CREATION DU PROFIL AXIAL DE FLUX   #
        ########################################
        _FLUXAX1 = DEFI_FONCTION( NOM_PARA    = 'X',
               VALE           = (self.alt_g1,         0.54,
                                 self.alt_g2,         1.,
                                 self.alt_gm - 0.001, 1.,
                                 self.alt_gm,         0.85,
                                 self.alt_gn,         0.57,),
                                 PROL_DROITE = 'CONSTANT',
                                 PROL_GAUCHE = 'CONSTANT',);

        #########################################################
        #   DEFINITION DU CHAMP NEUTRONIQUE RADIAL (CONSTANT)        #
        #########################################################
        Y_1= -1.0;
        Y_2=  1.0;

        _FLY_1=DEFI_FONCTION(NOM_PARA='Y', VALE=(Y_1,1.0,Y_2,1.0), PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',);

        _FLY_2=DEFI_FONCTION(NOM_PARA='Y', VALE=(Y_1,1.0,Y_2,1.0), PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',);

        _FLUXRD1=DEFI_NAPPE(NOM_PARA='Z', PARA=(Y_1,Y_2), FONCTION=(_FLY_1,_FLY_2), PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',);

        #------------------------------------------------
        # CREATION DU CHAMP ASSOCIE A LA FONCTION FLUXAX1
        #------------------------------------------------
        _CH_FAX = CREA_CHAMP(OPERATION='AFFE', TYPE_CHAM='NOEU_NEUT_F', MAILLAGE = MAILLAGE,
                     AFFE=( _F(GROUP_MA = ('T_GUIDE','CRAYON','ELA'), NOM_CMP= 'X1', VALE_F= _FLUXAX1,),),);

        _CH_FAXR = CREA_CHAMP(OPERATION='EVAL', TYPE_CHAM='NOEU_NEUT_R', CHAM_F = _CH_FAX, CHAM_PARA = _CHXN);

        #-----------------------------------------------
        # CREATION DU CHAMP ASSOCIE A LA FONCTION FLUXRD1
        #-----------------------------------------------
        _CH_FRD = CREA_CHAMP(OPERATION='AFFE', TYPE_CHAM='NOEU_NEUT_F', MAILLAGE =MAILLAGE,
                     AFFE=(_F(TOUT = 'OUI', NOM_CMP = 'X2', VALE_F = _FLUXRD1),),);

        _CH_FRDR=CREA_CHAMP(OPERATION='EVAL', TYPE_CHAM='NOEU_NEUT_R', CHAM_F = _CH_FRD, CHAM_PARA = _CHXN);

        _MULT        = FORMULE(NOM_PARA=('X1','X2','INST'), VALE='X1*X2*INST');

        _CHRES  = CREA_CHAMP(OPERATION='AFFE', TYPE_CHAM='NOEU_NEUT_F', MAILLAGE = MAILLAGE,
                     AFFE=(_F(GROUP_MA = ('T_GUIDE','CRAYON','ELA'), NOM_CMP = 'X1',VALE_F = _MULT),),);

        #-----------------------------------------------------
        # CREATION DU CHAMP FLUENC1 ASSOCIE A LA LISTE LINST
        #-----------------------------------------------------

        _INST_0=CREA_CHAMP(OPERATION='AFFE', TYPE_CHAM='NOEU_INST_R', MAILLAGE=MAILLAGE,
                   AFFE=(_F(GROUP_MA = ('T_GUIDE','CRAYON','ELA'), NOM_CMP = 'INST', VALE = 0.0),),);

        _REST_0=CREA_CHAMP(OPERATION='EVAL', TYPE_CHAM='NOEU_NEUT_R', CHAM_F=_CHRES,
                   CHAM_PARA=(_CH_FAXR,_CH_FRDR,_INST_0,));

        _RES_0 =CREA_CHAMP(OPERATION='ASSE', TYPE_CHAM='NOEU_IRRA_R', MAILLAGE=MAILLAGE,
                   ASSE=(_F(GROUP_MA = ('T_GUIDE','CRAYON','ELA'), CHAM_GD = _REST_0, NOM_CMP = 'X1', NOM_CMP_RESU = 'IRRA',),),);

        _INST_1=CREA_CHAMP(OPERATION='AFFE', TYPE_CHAM='NOEU_INST_R', MAILLAGE=MAILLAGE,
                   AFFE=(_F(GROUP_MA = ('T_GUIDE','CRAYON','ELA'), NOM_CMP = 'INST', VALE = fluence),),);

        _REST_1=CREA_CHAMP(OPERATION='EVAL', TYPE_CHAM='NOEU_NEUT_R', CHAM_F=_CHRES,
                   CHAM_PARA=(_CH_FAXR,_CH_FRDR,_INST_1,));

        _RES_1 =CREA_CHAMP(OPERATION='ASSE', TYPE_CHAM='NOEU_IRRA_R', MAILLAGE=MAILLAGE,
                   ASSE=(_F(GROUP_MA = ('T_GUIDE','CRAYON','ELA'), CHAM_GD = _REST_1, NOM_CMP = 'X1', NOM_CMP_RESU = 'IRRA',),),);

        _FLUENC=CREA_RESU(TYPE_RESU='EVOL_VARC', NOM_CHAM='IRRA', OPERATION='AFFE',
                 AFFE=( _F( CHAM_GD = _RES_0, INST = self.temps_simu['T0'],PRECISION=1.E-6),
                    _F( CHAM_GD = _RES_0, INST = self.temps_simu['T4'],PRECISION=1.E-6),
                    _F( CHAM_GD = _RES_1, INST = self.temps_simu['T5'],PRECISION=1.E-6),
                    _F( CHAM_GD = _RES_1, INST = self.temps_simu['T9'],PRECISION=1.E-6),),);

        return _FLUENC

    def definition_champ_temperature(self,MAILLAGE):
        from Accas import _F
        CREA_CHAMP    = self.macro.get_cmd('CREA_CHAMP')
        DEFI_FONCTION = self.macro.get_cmd('DEFI_FONCTION')
        CREA_RESU     = self.macro.get_cmd('CREA_RESU')

        ##############################################################
        # Temperatures utiles pour les calculs sous flux neutronique #
        ##############################################################
        # Temperature de reference #
        ############################
        TP_REF   = 20. ;
        ARRET_FR = 60.0 ;   # arret a froid (temp moyenne cuve)
        ARRET_CH = 290.0 ;  # arret a chaud (297.2 dans doc TF JD DC 1494)
                    # c est une temperature moyenne en cuve

        # profil lineaire de temperature pour les TG
        TP_TG1 = 288.8 ;    # temperature TG pour xinft
        TP_TG2 = 324.2 ;    # temperature TG pour xsupt

        ######################################################
        #   DEFINITION DES TEMPERATURES NODALES EVOLUTIVES   #
        ######################################################
        #   TEMPERATURE DE REFERENCE (A L'ARRET)         #
        ######################################################

        _F_TP1_1 = DEFI_FONCTION( NOM_PARA = 'X', NOM_RESU = 'TEMP', PROL_DROITE = 'CONSTANT', PROL_GAUCHE = 'CONSTANT',
                      VALE     = (self.XINFT, TP_REF, self.XSUPT, TP_REF),);

        ######################################################
        #    AFFECTATION DE REFENCE DU CHAMP DE TEMPERATURE  #
        #    D UN AC (A l'ARRET)                 #
        ######################################################

        _CHTEM11 = CREA_CHAMP( TYPE_CHAM = 'NOEU_TEMP_F', MAILLAGE=MAILLAGE, OPERATION = 'AFFE',
                       AFFE  =( _F(GROUP_NO = ('T_GUIDE','EBOSUP','EBOINF','CRAYON','ELA','DIL'), NOM_CMP = 'TEMP', VALE_F = _F_TP1_1,),),);

        ######################################################
        #   TEMPERATURE EN PHASE ARRET A FROID           #
        ######################################################

        _F_TP2_1 = DEFI_FONCTION( NOM_PARA = 'X', NOM_RESU = 'TEMP', PROL_DROITE = 'CONSTANT', PROL_GAUCHE = 'CONSTANT',
                                      VALE     = (self.XINFT, ARRET_FR, self.XSUPT, ARRET_FR),);

        ######################################################
        #    AFFECTATION DE REFENCE DU CHAMP DE TEMPERATURE  #
        #    D UN AC PENDANT LA PHASE D'ARRET A FROID             #
        ######################################################

        _CHTEM21 = CREA_CHAMP( TYPE_CHAM = 'NOEU_TEMP_F',MAILLAGE=MAILLAGE,OPERATION = 'AFFE',
                                   AFFE      = (_F(GROUP_NO = ('T_GUIDE','EBOSUP','EBOINF','CRAYON','ELA','DIL'), NOM_CMP = 'TEMP', VALE_F = _F_TP2_1,),),);

        ######################################################
        #   TEMPERATURE EN PHASE ARRET A CHAUD           #
        ######################################################

        _F_TP3_1 = DEFI_FONCTION( NOM_PARA = 'X', NOM_RESU = 'TEMP', PROL_DROITE = 'CONSTANT', PROL_GAUCHE = 'CONSTANT',
                                      VALE     =(self.XINFT, ARRET_CH, self.XSUPT, ARRET_CH),);

        ######################################################
        #    AFFECTATION DE REFENCE DU CHAMP DE TEMPERATURE  #
        #    D UN AC PENDANT LA PHASE D'ARRET A CHAUD             #
        ######################################################

        _CHTEM31 = CREA_CHAMP( TYPE_CHAM = 'NOEU_TEMP_F',MAILLAGE=MAILLAGE,OPERATION = 'AFFE',
                                   AFFE      = (_F(GROUP_NO = ('T_GUIDE','EBOSUP','EBOINF','CRAYON','ELA','DIL'), NOM_CMP = 'TEMP', VALE_F = _F_TP3_1,),),);

        ######################################################
        #   EVOLUTION DE LA TEMPERATURE DANS LES CRAYONS     #
        #   PENDANT LA PHASE D'IRRADIATION             #
        ######################################################

        XX1 =            self.XINFC ;
        XX2 = XX1 + self.LONCR*0.125 ;
        XX3 = XX1 + self.LONCR*0.875 ;
        XX4 = XX1 + self.LONCR ;

        _F_CR3 = DEFI_FONCTION( NOM_PARA = 'X', NOM_RESU    = 'TEMP', PROL_DROITE = 'LINEAIRE', PROL_GAUCHE = 'LINEAIRE',
                                    VALE     = ( XX1, 300., XX2, 330., XX3, 365., XX4, 340.),);

        ######################################################
        #   EVOLUTION DE LA TEMPERATURE DANS LES TUBES-GUIDE #
        #   ET AUTRES COMPOSANTS EN PHASE D'IRRADIATION      #
        ######################################################

        _F_TP4_1 = DEFI_FONCTION( NOM_PARA = 'X', NOM_RESU = 'TEMP', PROL_DROITE = 'CONSTANT', PROL_GAUCHE = 'CONSTANT',
                                      VALE     =(self.XINFT, TP_TG1, self.XSUPT, TP_TG2),);

        _CHTEM41 = CREA_CHAMP( TYPE_CHAM = 'NOEU_TEMP_F', MAILLAGE = MAILLAGE, OPERATION = 'AFFE',
                       AFFE  = ( _F(GROUP_NO = ('T_GUIDE','EBOSUP','EBOINF','ELA','DIL'), NOM_CMP = 'TEMP', VALE_F = _F_TP4_1,),
                                                 _F(GROUP_NO = 'CRAYON',                                  NOM_CMP = 'TEMP', VALE_F = _F_CR3,),),);

        _CHTH_1 = CREA_RESU( TYPE_RESU = 'EVOL_THER', NOM_CHAM  = 'TEMP', OPERATION = 'AFFE',
                     AFFE      = ( _F(CHAM_GD = _CHTEM11, INST = self.temps_simu['T0'], PRECISION=1.E-6),
                                               _F(CHAM_GD = _CHTEM11, INST = self.temps_simu['T1'], PRECISION=1.E-6),
                                               _F(CHAM_GD = _CHTEM21, INST = self.temps_simu['T2'], PRECISION=1.E-6),
                                               _F(CHAM_GD = _CHTEM31, INST = self.temps_simu['T3'], PRECISION=1.E-6),
                                               _F(CHAM_GD = _CHTEM41, INST = self.temps_simu['T4'], PRECISION=1.E-6),
                                               _F(CHAM_GD = _CHTEM41, INST = self.temps_simu['T5'], PRECISION=1.E-6),
                                               _F(CHAM_GD = _CHTEM31, INST = self.temps_simu['T6'], PRECISION=1.E-6),
                                               _F(CHAM_GD = _CHTEM21, INST = self.temps_simu['T7'], PRECISION=1.E-6),
                                               _F(CHAM_GD = _CHTEM11, INST = self.temps_simu['T8'], PRECISION=1.E-6),
                           _F(CHAM_GD = _CHTEM11, INST = self.temps_simu['T9'], PRECISION=1.E-6),),);

        return _CHTH_1

    def definition_materiau(self,MAILLAGE,GFF,CONTACT,FLUENCE,CHTH):
        from Accas import _F
        DEFI_COMPOR  = self.macro.get_cmd('DEFI_COMPOR')
        DEFI_MATERIAU         = self.macro.get_cmd('DEFI_MATERIAU')
        AFFE_MATERIAU         = self.macro.get_cmd('AFFE_MATERIAU')

        TP_REF = 20. ;


        if (CONTACT == 'OUI'):
           _M_RES  = DEFI_MATERIAU( DIS_CONTACT = _F( RIGI_NOR = 1.E9, ),);
        elif (CONTACT == 'NON'):
           _M_RES  = DEFI_MATERIAU( DIS_CONTACT = _F( RIGI_NOR = 1.E4, ),);

        mcf_affe_mater = self.mcf_coeur_mater(_M_RES)
        mcf_compor     = self.mcf_compor_fibre(GFF)

        # Affectation des materiau dans le coeur

        _A_MAT = AFFE_MATERIAU( MAILLAGE    = MAILLAGE,
                        AFFE_VARC   = ( _F( NOM_VARC='IRRA', TOUT='OUI', EVOL=FLUENCE, PROL_DROITE='CONSTANT'),
                                        _F( NOM_VARC='TEMP', TOUT='OUI', EVOL=CHTH,    PROL_DROITE='CONSTANT', VALE_REF=TP_REF,),),
                        AFFE            = mcf_affe_mater,
                        AFFE_COMPOR = mcf_compor,);

        return _A_MAT

    def mcf_compor_fibre(self,GFF):
        from Accas import _F
        DEFI_COMPOR = self.macro.get_cmd('DEFI_COMPOR')
        mcf  = []
        for ac in self.collAC.values():
            _CMPC = DEFI_COMPOR( GEOM_FIBRE =  GFF,
                                 MATER_SECT =  ac.mate.mate['CR'],
                                 MULTIFIBRE = _F( GROUP_FIBRE =  'CR_' + ac.idAST,
                                                  MATER       =  ac.mate.mate['CR'],
                                                  RELATION    =  'GRAN_IRRA_LOG',
                                                  DEFORMATION = 'GROT_GDEP',),)
            _CMPT = DEFI_COMPOR( GEOM_FIBRE =  GFF,
                                 MATER_SECT =  ac.mate.mate['TG'],
                                 MULTIFIBRE = _F( GROUP_FIBRE =  ('LG_' + ac.idAST, 'BI_' + ac.idAST, 'RE_' + ac.idAST,),
                                                  MATER       =  ac.mate.mate['TG'],
                                                  RELATION    =  'GRAN_IRRA_LOG',
                                                  DEFORMATION = 'GROT_GDEP',),)
            mtmp = (_F(GROUP_MA = 'CR_' + ac.idAST, COMPOR = _CMPC,),
                        _F(GROUP_MA = 'TG_' + ac.idAST, COMPOR = _CMPT,),)
            mcf.extend(mtmp)

        return mcf

    def mcf_coeur_mater(self,_M_RES):
        from Accas import _F
        DEFI_MATERIAU = self.macro.get_cmd('DEFI_MATERIAU')
        # Definition d'un materiau bidon pour les elements de poutres
        _MAT_BID = DEFI_MATERIAU(ELAS = _F( E = 1.0,   NU = 0.0, RHO = 0.0, ALPHA = 0.0,),);
        _MAT_GR  = DEFI_MATERIAU(ELAS = _F( E = 1.E14, NU = 0.3, RHO = 0.0, ALPHA = 0.0,),);

        mcf  = []
        mtmp = (_F(GROUP_MA = ('RES_EXT','RES_CONT'), MATER = _M_RES,),)
        mcf.extend(mtmp)

        for ac in self.collAC.values():
            mcf.extend(ac.mcf_AC_mater())
            mtmp = (_F(GROUP_MA = ('GT_' + ac.idAST + '_M','GT_' + ac.idAST + '_E',), MATER = _MAT_BID,),
                    _F(GROUP_MA =  'GR_' + ac.idAST,                                  MATER = _MAT_GR,),)
            mcf.extend(mtmp)
            # ATTENTION ici on definit pour tout le group_ma 'DIL' le materiau de type ac.collAC.
            # Cette affectation concerne le calcul avec dilatation thermique des grilles et de la cuve.
            # C'est donc le dernier qui sera pris en compte car aujourd'hui on considere que l'ensemble
            # se dilate de la meme facon.
            # On repete en ecrasant a chaque fois avec la meme valeur pour tout le groupe DIL
        mtmp = (_F(GROUP_MA = 'DIL', MATER = ac.mate.mate['DIL'],),)
        mcf.extend(mtmp)
        return mcf


class CoeurFactory(Mac3Factory):
    """Classe pour construire les objets Coeur."""
    # Ex.: La classe "Coeur" sera nommée Coeur_900 dans le fichier Coeur_900.datg
    prefix = 'Coeur_'

    def build_supported_types(self):
        """Construit la liste des types autorisés."""
        ctxt = {}
        for obj, val in globals().items():
            if type(val) is type and issubclass(val, Coeur):
                ctxt[obj] = val
        return ctxt


class MateriauAC(object):
    """Conteneur des matériaux d'un assemblage."""
    _types = ('DIL','ES', 'EI', 'CR', 'TG', 'GC_ME', 'GC_EB', 'GC_EH')

    def __init__(self, typeAC, macro):
        """Initialisation"""
        self.typeAC = typeAC
        self.macro = macro
        self.mate = {}.fromkeys(self._types)
        self.include_materiau()

    def include_materiau(self):
        """Crée les matériaux"""
        INCLUDE_MATERIAU = self.macro.get_cmd("INCLUDE_MATERIAU")

        for typ in self._types:
            _mat = INCLUDE_MATERIAU(NOM_AFNOR   = self.typeAC + '_' + typ,
                                    TYPE_MODELE = 'REF',
                                    VARIANTE    = 'A',
                                    TYPE_VALE   = 'NOMI')
            self.mate[typ] = _mat

