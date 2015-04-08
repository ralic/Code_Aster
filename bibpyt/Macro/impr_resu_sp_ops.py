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
#
# person_in_charge: jean-luc.flejou at edf.fr
#

def Table2vtu(nomfichtar,Reper,LignepvdData):
    #
    import string
    import os.path as OSP
    import tarfile
    #
    # LignepvdData[Resultat-Champ-iocc] : [ (Fichier  Resultat  Champ  LComposante  Nume_Ordre  instant), .... ]
    #
    #           Fichier = Resultat-Champ-occ-Nume_Ordre
    #
    # Compactage de tous les fichiers dans le ".tar"
    letar = tarfile.open(nomfichtar, 'w')
    #
    # Boucle sur les "Resultat-Champ"
    for Resu_Champ in LignepvdData.keys():
        # Les fichier pvd
        Lignepvd = ['<?xml version="1.0"?>']
        Lignepvd.append('<VTKFile type="Collection" version="0.1" byte_order="LittleEndian">')
        Lignepvd.append('   <Collection>')
        for Fichier,Resultat,Champ,Composante,Nume_Ordre,instant in LignepvdData[Resu_Champ]:
            Lignepvd.append('      <DataSet timestep="%f" group="" part="0" file="%s.vtu"/>' % (instant,Fichier) )
        Lignepvd.append('   </Collection>')
        Lignepvd.append('</VTKFile>')
        #
        #
        ListeFichier = [ '%s.pvd' % Resu_Champ ]
        fic = open( OSP.join( Reper , ListeFichier[-1] ) ,'w' )
        fic.write( '\n'.join(Lignepvd) )
        fic.close()
        #
        # Traitement des fichiers Tables
        for Fichier,Resultat,Nom_Champ,LNomCmp,Nume_Ordre,instant in LignepvdData[Resu_Champ]:
            Unetable = OSP.join( Reper,  '%s.table' % Fichier )
            #
            fic = open(Unetable,'r')
            leslignes = fic.readlines()
            fic.close()
            #
            nbcmp = 0
            Pointxyz  = ['\n<Points>']
            Pointxyz.append('<DataArray type="Float64" NumberOfComponents="3" format="ascii">')
            #
            CellsConnec = ['\n<DataArray type="Int64" Name="connectivity" format="ascii">']
            CellsOffset = ['\n<DataArray type="Int64" Name="offsets" format="ascii">']
            CellsTypes  = ['\n<DataArray type="UInt8" Name="types" format="ascii" RangeMin="1" RangeMax="1">']
            #
            nbpoint = 0
            for uneligne in leslignes:
                tmp = uneligne.strip()
                try:
                    tmp = tmp.split()
                    if ( len(tmp) == 0): continue
                    valeur = map( float, tmp )
                    #
                    assert nbcmp <> 0
                    CellsConnec.append('%8d' %(nbpoint) )
                    CellsOffset.append('%8d' %(nbpoint+1) )
                    CellsTypes.append(' 1' )
                    #
                    Pointxyz.append(' '.join( tmp[:3] ) )
                    PointData.append(' '.join( tmp[3:] ) )
                    nbpoint+=1
                except:
                    tmp = uneligne.strip()
                    if ( tmp[:4] == 'COOR'):
                        tmp = tmp.split()
                        nbcmp =len(tmp)-3
                        PointData = ['\n<PointData Scalars="%s">' % Nom_Champ.upper()]
                        ComponentName=''
                        for ii in range(nbcmp):
                            ComponentName+='ComponentName%d="%s" ' % (ii,LNomCmp[ii])
                        PointData.append('<DataArray type="Float64" NumberOfComponents="%d" %s Name="%s" format="ascii">' % (nbcmp,ComponentName,Nom_Champ.upper()) )
                    pass
            #
            Entete = ['<VTKFile type="UnstructuredGrid" version="0.1" byte_order="LittleEndian">']
            Entete.append('<UnstructuredGrid>')
            Entete.append('<Piece NumberOfPoints="%8d" NumberOfCells="%8d">'%(nbpoint,nbpoint) )
            #
            PointData.append('</DataArray>')
            PointData.append('</PointData>')
            #
            CellData = ['\n<CellData>']
            CellData.append('</CellData>')
            #
            Pointxyz.append('</DataArray>')
            Pointxyz.append('</Points>')
            #
            CellsConnec.append('</DataArray>')
            CellsOffset.append('</DataArray>')
            CellsTypes .append('</DataArray>')
            #
            ListeFichier.append( '%s.vtu' % Fichier )
            ficout = open( OSP.join( Reper, ListeFichier[-1] ) ,'w')
            ficout.write('\n'.join(Entete))
            ficout.write('\n'.join(PointData))
            ficout.write('\n'.join(CellData))
            ficout.write('\n'.join(Pointxyz))
            ficout.write('\n<Cells>')
            ficout.write('\n'.join(CellsConnec))
            ficout.write('\n'.join(CellsOffset))
            ficout.write('\n'.join(CellsTypes))
            ficout.write('\n</Cells>')
            ficout.write('\n</Piece>')
            ficout.write('\n</UnstructuredGrid>')
            ficout.write('\n</VTKFile>')
            ficout.close()
        #
        for nomfic in  ListeFichier:
            letar.add( OSP.join(Reper, nomfic),arcname=nomfic,recursive=False )
    # Fermeture du ".tar"
    letar.close()
    #
    return


def DedansListe(vale , laliste, precision=1.0E-03 ):
    for ixx in range(len(laliste)):
        if ( abs(laliste[ixx]-vale) <= precision ): return ixx
    return -1.0


def impr_resu_sp_ops(self,
    RESULTAT, NUME_ORDRE, INST, LIST_INST,
    GROUP_MA, RESU, UNITE, **args):
    """
       Visualisation des sous-points pour MED
    """
    import os.path as OSP
    import numpy as NP
    from Utilitai.Utmess import UTMESS
    from Utilitai.UniteAster import UniteAster
    import string as ST
    import tempfile
    from Accas import _F
    #
    ier = 0
    # On importe les définitions des commandes à utiliser dans la macro
    DEFI_FICHIER = self.get_cmd('DEFI_FICHIER')
    CREA_TABLE   = self.get_cmd('CREA_TABLE')
    IMPR_TABLE   = self.get_cmd('IMPR_TABLE')
    DETRUIRE     = self.get_cmd('DETRUIRE')
    # La macro compte pour 1 dans la numérotation des commandes
    self.set_icmd(1)
    # Pas de concept sortant
    #
    # Extraction des INST, NUME_ORDRE, CHAMP du résultat
    Resultemps=RESULTAT.LIST_PARA()['INST']
    Resulordre=RESULTAT.LIST_PARA()['NUME_ORDRE']
    ResuName = RESULTAT.nom
    #
    # lestemps : les temps d'extraction des champs
    if   ( INST != None ):
        lestempsextrac = INST
    elif ( LIST_INST != None ):
        lestempsextrac = LIST_INST.Valeurs()
    #
    # Fabrique de list_numeordre_instant = [ (nume_ordre, temps), ... ]
    if ( NUME_ORDRE != None ):
        tmp = []
        for nume in NUME_ORDRE:
            if ( nume in Resulordre ):
                indx = Resulordre.index( nume )
                tmp.append( (Resulordre[indx],Resultemps[indx]) )
            else:
                valk = ( ResuName, )
                vali = ( nume,)
                UTMESS('F','IMPRRESUSP_1', valk=valk, vali=vali)
        list_numeordre_instant = NP.array( tmp )
    elif ( INST != None or LIST_INST != None ):
        tmp = []
        for inst in lestempsextrac:
            indx = DedansListe( inst, Resultemps, precision=1.0E-08 )
            if ( indx >= 0 ):
                tmp.append( (Resulordre[indx],Resultemps[indx]) )
            else:
                valk = ( ResuName, )
                valr = ( inst, )
                UTMESS('F','IMPRRESUSP_2', valk=valk, valr=valr)
        list_numeordre_instant = NP.array( tmp )
    else:
        list_numeordre_instant = NP.transpose(NP.array([Resulordre , Resultemps]))
    # Triage suivant les nume_ordre croissant
    list_numeordre_instant[list_numeordre_instant[:,0].argsort()]
    # Les champs et leur composantes
    Resulchamp=RESULTAT.LIST_CHAMPS()
    Resulcompo=RESULTAT.LIST_NOM_CMP()
    #
    LesChampsComposantes = []
    motclef = RESU.List_F()
    for ii in range(len(motclef)):
        mclf = motclef[ii]
        nom_cham = mclf['NOM_CHAM']
        nom_cmp  = mclf['NOM_CMP']
        # Champ dans le RESULTAT ?
        if ( not nom_cham in Resulchamp.keys() ):
            valk = ( ResuName, nom_cham )
            UTMESS('F','IMPRRESUSP_3', valk=valk)
        # Composante dans le CHAMP ?
        for ncmp in nom_cmp:
            if ( not ncmp in Resulcompo[nom_cham] ):
                valk = ( ResuName, nom_cham, ncmp  )
                UTMESS('F','IMPRRESUSP_4', valk=valk)
        # nume_ordre existe pour le champ ?
        for nume,inst in list_numeordre_instant:
            if ( not int(nume) in Resulchamp[nom_cham] ):
                valk = ( ResuName, nom_cham )
                vali = ( nume, )
                valr = ( inst, )
                UTMESS('F','IMPRRESUSP_5', valk=valk, vali=vali, valr=valr)
        #
        LesChampsComposantes.append( ( ii+1, nom_cham , list(nom_cmp) ) )
    #
    Group_MA = list( GROUP_MA )
    #
    # création du répertoire
    RepertoireSauve = tempfile.mkdtemp( prefix='Visu_Sous_Point', dir='.' )
    # On commence la macro
    LignepvdData = {}
    for UnChamp in LesChampsComposantes:
        iocc, Nom_Champ, LNom_Cmp = UnChamp
        #
        # Liste informations : [ (Fichier  Resultat  Champ  Composante  Nume_Ordre  instant), .... ]
        clef = '%s-%s-%04d' % (ResuName, Nom_Champ, iocc)
        LignepvdData[clef] = []
        # Boucle sur les numéros d'ordre
        for nume_ordre_instant in list_numeordre_instant:
            nordre, instant = nume_ordre_instant
            nume_ordre = int( nordre )
            Nom_Fic= '%s-%04d' % (clef ,nume_ordre)
            #
            LignepvdData[clef].append( (Nom_Fic, ResuName, Nom_Champ, LNom_Cmp, nume_ordre, instant) )
            #
            __unit = DEFI_FICHIER(ACTION='ASSOCIER', FICHIER=OSP.join(RepertoireSauve,'%s.table' % Nom_Fic), ACCES='NEW', TYPE='ASCII')
            __tbresu=CREA_TABLE(
                RESU=_F(RESULTAT=RESULTAT, NOM_CHAM=Nom_Champ.upper() , NUME_ORDRE=nume_ordre, GROUP_MA=Group_MA, NOM_CMP=LNom_Cmp,),)
            IMPR_TABLE(FORMAT='TABLEAU', UNITE=__unit, TABLE=__tbresu, NOM_PARA=['COOR_X','COOR_Y','COOR_Z'] + LNom_Cmp,)
            DEFI_FICHIER(ACTION='LIBERER', UNITE=__unit)
            DETRUIRE(CONCEPT=_F(NOM=__tbresu,), INFO=1,)
            DETRUIRE(CONCEPT=_F(NOM=__unit,), INFO=1,)
        #
    #
    # Fichier de l'unité logique UNITE
    UL = UniteAster()
    nomfich = UL.Nom(UNITE)
    # Fabrication du pvd et des vtu dans un fichier "tgz"
    Table2vtu(nomfich,RepertoireSauve,LignepvdData)
    # Remet UNITE dans son état initial
    UL.EtatInit()
    return ier