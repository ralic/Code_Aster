# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: jacques.pellet@edf.fr

# --------------------------------------------------------------------------------
#       impression des catalogues d'elements au format "jeveux" (ojb)
# ------------------------------------------------------------------------
import string
import copy
import os
import os.path as osp
import re
import traceback
import copy
from collections import OrderedDict

from cataelem.Commons import attributes as AT
from cataelem.Tools import jeveux_utils as JV

ERR = JV.ERR


# --------------------------------------------------------------------------------
# Remarque : Ce fichier contient des "bouts" de code que l'on peut facilement executer
#            pour obtenir des fichiers de documentation ou de problemes.
#            Ces bouts de code se situent ci-dessous vers la chaine "XXUTIL"
# ------------------------------------------------------------------------
#
# Fonction principale :
#
def impr_cata(cel, nomfic, timer, dbgdir=None):
#============================
# imprimer les catalogues d'elements (cel) sur le fichier (nomfic) au
# format "objets jeveux"

    fimpr = open(nomfic, "w")
    imprime_ojb(cel, fimpr, timer, dbgdir)
    fimpr.close()
    ERR.fini()

#
# utilitaires :
#
def txtpad(long, chaine):
    #---------------------------------------
    # retourne une chaine de longueur "long" en completant chaine par des
    # "blancs"
    return chaine[:long].ljust(long)


#-------------------------------------------------------------------------
# impression au format 'ojb' :
#-------------------------------------------------------------------------
def imprime_ojb(cel, file, timer, dbgdir):
    timer.Start('T1')
    ERR.mess(
        'I', "Debut de la transformation de l'ENSEMBLE des catalogues en objets jeveux")

    d = {}  # dictionnaire des ojb

    #=========================================================================
    # Bouts de code pouvant servir aux developpeurs pour generer des fichiers pratiques pour les scripts :
    # Ces bouts de code sont places ici, avant les "del cata"
    if dbgdir:
        # pour imprimer tous les 6-uplets :
        # ( OPTION  TYPELEM  IN/OUT  PARAM  GRANDEUR  CMP )
        impr_CMP(osp.join(dbgdir, "CATA_liCMP.txt"), cel)
        # le fichier produit est moins gros que liCMP mais surtout il contient
        # les paramètres RESL  pour imprimer tous les 5-uplets
        # ( OPTION  TYPELEM  IN/OUT  PARAM  GRANDEUR)
        impr_param_options(osp.join(dbgdir, "CATA_param_options.txt"), cel)
        # pour imprimer les lignes (type_elem, type_maille, attribut1,
        # attribut2, ... )
        nomte_nomtm(osp.join(dbgdir, "CATA_nomte_nomtm.txt"), cel)
        # pour imprimer les lignes (te00ij -> (type_elem1, type_elem2, ...)
        numte_lnomte(osp.join(dbgdir, "CATA_numte_lnomte.txt"), cel)

    #=========================================================================
    # Verifications de coherence des catalogues :
    #--------------------------------------------
    verif_phenmode(cel)

    #=========================================================================

    #  TOUCOMLIBR = objet contenant tous les commentaires libres :
    #-------------------------------------------------------------
    TOUCOMLIBR = JV.cree_co(
        d, nom='&CATA.CL.COMLIBR', tsca='K80', tsca_pn='K8', contig='CONTIG', acces='NU', longv=1)

    def split_comlibr(TOUCOMLIBR, comlibr):
        # "splite" la chaine comlibr en plusieurs lignes et stocke ces lignes dans TOUCOMLIBR
        indice = len(TOUCOMLIBR.objs)
        if comlibr:
            l = string.split(comlibr, '\n')
            nblig = len(l)
            ind1 = indice
            for x in l:
                ind1 = ind1 + 1
                TOUCOMLIBR.cree_oc(nom=str(ind1), long=1)
                TOUCOMLIBR.ecri_co(nom=str(ind1), indice=1, valeur=x)
        else:
            nblig = 0
        return (nblig, indice + 1)

    def elrefe_npg(NOFPG, elrf, nfam):
        # retourne le nombre de points de la famille f de l'elrefe elrf et le
        # numero de la famille
        npg = elrf.locations.get(nfam)
        if npg:
            ifpg = NOFPG.jenonu(txtpad(8, elrf.name) + nfam)
            return (int(npg), ifpg)
        else:
            ERR.mess('E', " famille de Points de Gauss inconnue: " +
                     nfam + " pour ELREFE: " + elrf)

    #  catalogue des TYPE_MAILLE :
    #-----------------------------------------
    timer.Stop('T1')
    timer.Start('T2')
    ERR.contexte("Examen du catalogue des types de mailles")
    NOMTM = JV.cree_pn(d, nom='&CATA.TM.NOMTM', tsca='K8')
    NOELRF = JV.cree_pn(d, nom='&CATA.TM.NOELRF', tsca='K8')
    NOFPG = JV.cree_pn(d, nom='&CATA.TM.NOFPG', tsca='K16')
    NBNO = JV.cree_co(d, nom='&CATA.TM.NBNO', tsca='I',
                      tsca_pn='K8', contig='CONTIG', acces='NO', longv=1)
    TMDIM = JV.cree_co(d, nom='&CATA.TM.TMDIM', tsca='I',
                       tsca_pn='K8', contig='CONTIG', acces='NO', longv=1)
    nb_elrf = 0
    nb_fpg = 0
    meshTypes = cel.getMeshTypes()
    nbtm = len(meshTypes)
    for tm in meshTypes:
        notm = tm.name
        NOMTM.ajout_nom(notm)
        NBNO.cree_oc(nom=notm, long=1)
        NBNO.ecri_co(nom=notm, indice=1, valeur=int(tm.nbNodes))
        TMDIM.cree_oc(nom=notm, long=1)
        TMDIM.ecri_co(nom=notm, indice=1, valeur=int(tm.dim))
        for elrf in tm.getElrefe():
            nom = elrf.name
            assert nom not in NOELRF.dico
            nb_elrf = nb_elrf + 1
            NOELRF.ajout_nom(nom)
            nom = txtpad(8, nom)
            for nfam in elrf.locations:
                nb_fpg = nb_fpg + 1
                NOFPG.ajout_nom(nom + nfam)

    TMELRF = JV.cree_os(d, nom='&CATA.TM.TMELRF', tsca='I', long=nb_elrf)
    TMFPG = JV.cree_os(d, nom='&CATA.TM.TMFPG', tsca='I', long=nb_fpg)
    ifpg = 0
    for tm in meshTypes:
        notm = tm.name
        nutyma = NOMTM.jenonu(notm)
        for elrf in tm.getElrefe():
            nom = elrf.name
            ielrf = NOELRF.jenonu(nom)
            TMELRF.ecri_os(indice=ielrf, valeur=nutyma)
            for nfam, npg in elrf.locations.items():
                npg = int(npg)
                ifpg = ifpg + 1
                TMFPG.ecri_os(indice=ifpg, valeur=npg)

    #  catalogue des grandeurs :
    #-----------------------------------------
    timer.Stop('T2')
    timer.Start('T3')
    ERR.contexte("Examen du catalogue des grandeurs")

    # calcul de l_gdsimp et l_gdelem :
    l_gdsimp = cel.getPhysicalQuantities()
    l_gdelem = cel.getArrayOfQuantities()

    nbgd = len(l_gdsimp) + len(l_gdelem)
    NOMGD = JV.cree_pn(d, nom='&CATA.GD.NOMGD', tsca='K8')
    TYPEGD = JV.cree_os(d, nom='&CATA.GD.TYPEGD', tsca='K8', long=nbgd)
    NOMCMP = JV.cree_co(d, nom='&CATA.GD.NOMCMP', tsca='K8',
                        tsca_pn='K8', contig='CONTIG', acces='NO', longv=0)
    DESCRIGD = JV.cree_co(d, nom='&CATA.GD.DESCRIGD', tsca='I',
                          tsca_pn='K8', contig='CONTIG', acces='NU', longv=7)

    k = 0
    for gd in l_gdsimp:
        k = k + 1
        components = gd.components
        ncmp = len(components)
        nogd = gd.name
        NOMGD.ajout_nom(nogd)
        TYPEGD.ecri_os(indice=k, valeur=gd.type)
        NOMCMP.cree_oc(nom=nogd, long=ncmp)
        DESCRIGD.cree_oc(nom=nogd, long=7)

        (nblcom, indcom) = split_comlibr(TOUCOMLIBR, gd.comment)
        DESCRIGD.ecri_co(nom=nogd, indice=6, valeur=nblcom)
        DESCRIGD.ecri_co(nom=nogd, indice=7, valeur=indcom)

        for icmp in range(ncmp):
            NOMCMP.ecri_co(nom=nogd, indice=icmp + 1, valeur=components[icmp])

        DESCRIGD.ecri_co(nom=nogd, indice=1, valeur=1)
        DESCRIGD.ecri_co(nom=nogd, indice=3, valeur=(ncmp - 1) / 30 + 1)

    for gd in l_gdelem:
        k = k + 1
        nogd = gd.name
        NOMGD.ajout_nom(nogd)
        NOMCMP.cree_oc(nom=nogd, long=0)
        DESCRIGD.cree_oc(nom=nogd, long=7)

        if gd.dim == 'V':
            ERR.veri_appartient_liste(
                'F', gd.physicalQuantity.name, NOMGD.valeurs)
            igd1 = NOMGD.valeurs.index(gd.physicalQuantity.name)
            DESCRIGD.ecri_co(nom=nogd, indice=1, valeur=3)
            DESCRIGD.ecri_co(nom=nogd, indice=4, valeur=igd1 + 1)
            tscal = TYPEGD.valeurs[igd1]

        elif gd.dim in ('MS', 'MR'):
            ERR.veri_appartient_liste(
                'F', gd.physicalQuantity.name, NOMGD.valeurs)
            igd1 = NOMGD.valeurs.index(gd.physicalQuantity.name)

            if gd.dim == 'MS':
                DESCRIGD.ecri_co(nom=nogd, indice=1, valeur=4)
            elif gd.dim == 'MR':
                DESCRIGD.ecri_co(nom=nogd, indice=1, valeur=5)

            DESCRIGD.ecri_co(nom=nogd, indice=4, valeur=igd1 + 1)
            DESCRIGD.ecri_co(nom=nogd, indice=5, valeur=igd1 + 1)
            tsca1 = TYPEGD.valeurs[igd1]
            tscal = tsca1

        else:
            assert 0, gd.dim

        TYPEGD.ecri_os(indice=k, valeur=tscal)

    #  catalogues des options :
    #-----------------------------------------
    timer.Stop('T3')
    timer.Start('T4')
    options = cel.getOptions()
    nbop = len(options)
    NOMOP = JV.cree_pn(d, nom='&CATA.OP.NOMOPT', tsca='K16')
    DESCOPT = JV.cree_co(d, nom='&CATA.OP.DESCOPT', tsca='I',
                         tsca_pn='K16', contig='CONTIG', acces='NU', longv=0)
    OPTPARA = JV.cree_co(d, nom='&CATA.OP.OPTPARA', tsca='K8',
                         tsca_pn='K16', contig='CONTIG', acces='NU', longv=0)
    LOCALIS = JV.cree_co(d, nom='&CATA.OP.LOCALIS', tsca='K24',
                         tsca_pn='K16', contig='CONTIG', acces='NU', longv=0)

    for opt in options:
        nom = opt.name
        comlibr = opt.comment
        lpain = opt.para_in
        lpaou = opt.para_out
        cond_calcul = opt.condition

        ERR.contexte("Examen du catalogue d'option: " + nom)
        nbin = len(lpain)
        nbou = len(lpaou)

        NOMOP.ajout_nom(nom)
        DESCOPT.cree_oc(nom=nom, long=6 + 3 * (nbin + nbou))
        OPTPARA.cree_oc(nom=nom, long=nbin + 2 * nbou)
        LOCALIS.cree_oc(nom=nom, long=3 * nbin)

        DESCOPT.ecri_co(nom=nom, indice=2, valeur=nbin)
        DESCOPT.ecri_co(nom=nom, indice=3, valeur=nbou)
        (nblcom, indcom) = split_comlibr(TOUCOMLIBR, comlibr)
        DESCOPT.ecri_co(nom=nom, indice=4 + nbin + nbou + 1, valeur=nblcom)
        DESCOPT.ecri_co(nom=nom, indice=4 + nbin + nbou + 2, valeur=indcom)

        k = 0
        for para_in in lpain:
            k = k + 1
            para = para_in.name
            comlibr = para_in.comment
            nogd = para_in.physicalQuantity.name
            localis = para_in.container

            igd = NOMGD.jenonu(nogd)
            DESCOPT.ecri_co(nom=nom, indice=4 + k, valeur=igd)
            OPTPARA.ecri_co(nom=nom, indice=k, valeur=para)
            if localis is not None:
                tabDep = localis.split("!")
                LOCALIS.ecri_co(nom=nom, indice=3 * k - 2, valeur=tabDep[0])
                LOCALIS.ecri_co(nom=nom, indice=3 * k - 1, valeur=tabDep[1])
                if len(tabDep) == 3:
                    LOCALIS.ecri_co(nom=nom, indice=3 * k, valeur=tabDep[2])
                else:
                    LOCALIS.ecri_co(nom=nom, indice=3 * k, valeur="NSP")
            else:
                LOCALIS.ecri_co(nom=nom, indice=3 * k - 2, valeur="VIDE")
                LOCALIS.ecri_co(nom=nom, indice=3 * k - 1, valeur="VIDE")
                LOCALIS.ecri_co(nom=nom, indice=3 * k, valeur="VIDE")
            (nblcom, indcom) = split_comlibr(TOUCOMLIBR, comlibr)
            DESCOPT.ecri_co(
                nom=nom, indice=6 + nbin + nbou + 2 * (k - 1) + 1, valeur=nblcom)
            DESCOPT.ecri_co(
                nom=nom, indice=6 + nbin + nbou + 2 * (k - 1) + 2, valeur=indcom)

        k = 0
        for para_ou in lpaou:
            k = k + 1
            para = para_ou.name
            comlibr = para_ou.comment
            nogd = para_ou.physicalQuantity.name
            typout = para_ou.type

            igd = NOMGD.jenonu(nogd)
            DESCOPT.ecri_co(nom=nom, indice=4 + nbin + k, valeur=igd)
            OPTPARA.ecri_co(nom=nom, indice=nbin + k, valeur=para)
            OPTPARA.ecri_co(nom=nom, indice=nbin + nbou + k, valeur=typout)
            (nblcom, indcom) = split_comlibr(TOUCOMLIBR, comlibr)
            DESCOPT.ecri_co(
                nom=nom, indice=6 + 3 * nbin + nbou + 2 * (k - 1) + 1, valeur=nblcom)
            DESCOPT.ecri_co(
                nom=nom, indice=6 + 3 * nbin + nbou + 2 * (k - 1) + 2, valeur=indcom)

    #  catalogues des type_elem :
    #-------------------------------------------
    # fonction de calcul d'une suite d'entiers codes correspondant à une liste
    # de CMPS:
    def entiers_codes(note, lcmp, lcmp_gd):
        nbec = (len(lcmp_gd) - 1) / 30 + 1
        liec = [0] * nbec
        rangav = -1
        for icmp in range(len(lcmp)):
            # ERR.veri_appartient_liste('F', lcmp[icmp], lcmp_gd)
            # index will raise IndexError
            rangcmp = lcmp_gd.index(lcmp[icmp])
            if rangcmp < rangav:
                ERR.mess('E', " CMPS dans un ordre incorrect. " +
                         repr(lcmp) + " type_elem: " + note)

            rangav = rangcmp
            iec = rangcmp / 30
            puiss = (rangcmp % 30) + 1
            liec[iec] = liec[iec] | 2 ** puiss
        return liec

#   --- debut instructions imprime_ojb pour les elements :
#   ------------------------------------------------------
    timer.Stop('T4')
    timer.Start('T5')
#   -- calcul de 2 dictionnaires qui seront utilises plus loin :
    opt_contrainte, opt_a_calculer = liste_opt_a_calculer(cel, dbgdir)

    timer.Stop('T5')
    timer.Start('T6')
    nbte = len(cel.getElements())
    nblocfpg = cel.getNbLocations()
    nbopte = calc_nbopte(cel, opt_a_calculer)
    if dbgdir:
        print "DEBUG:", [nbte, nblocfpg, nbopte]

    NOMTE = JV.cree_pn(d, nom='&CATA.TE.NOMTE', tsca='K16')
    TYPEMA = JV.cree_os(d, nom='&CATA.TE.TYPEMA', tsca='K8', long=nbte)
    NOMMOLOC = JV.cree_pn(d, nom='&CATA.TE.NOMMOLOC', tsca='K24')
    MODELOC = JV.cree_co(d, nom='&CATA.TE.MODELOC', tsca='I',
                         tsca_pn='K24', contig='CONTIG', acces='NU', longv=0)
    NBELREFE = JV.cree_os(d, nom='&CATA.TE.NBELREFE', tsca='I', long=2 * nbte)
    NOELREFE = JV.cree_os(
        d, nom='&CATA.TE.NOELREFE', tsca='K8', long=cel.getNbElrefe())
    PNLOCFPG = JV.cree_os(
        d, nom='&CATA.TE.PNLOCFPG', tsca='K32', long=nblocfpg)
    NOLOCFPG = JV.cree_os(d, nom='&CATA.TE.NOLOCFPG', tsca='I', long=nblocfpg)
    OPTT2 = JV.cree_os(d, nom='&CATA.TE.OPTT2', tsca='I', long=2 * nbopte)

    OPTMOD = JV.cree_co(d, nom='&CATA.TE.OPTMOD', tsca='I',
                        tsca_pn='K8', contig='CONTIG', acces='NU', longv=0)
    OPTNOM = JV.cree_co(d, nom='&CATA.TE.OPTNOM', tsca='K8',
                        tsca_pn='K8', contig='CONTIG', acces='NU', longv=0)
    CTE_ATTR = JV.cree_co(d, nom='&CATA.TE.CTE_ATTR', tsca='K16',
                          tsca_pn='K16', contig='CONTIG', acces='NU', longv=0)

    # objets FPG_LISTE et NOFPG_LISTE :
    # --------------------------------------
    ERR.contexte("fabrication de l'objet .FPG_LISTE")
    # locations={NOMTE(1:16)//nofpgl(1:8):[nofpg1,nofpg2,...,ELREFE]}
    locations = getAllLocations(cel)
    FPG_LISTE = JV.cree_co(d, nom='&CATA.TE.FPG_LISTE', tsca='K8',
                           tsca_pn='K24', contig='CONTIG', acces='NU', longv=0)
    NOFPG_LISTE = JV.cree_pn(d, nom='&CATA.TE.NOFPG_LISTE', tsca='K24')
    locIndex = {}
    i = 0
    for nofpgl, loc in locations.items():
        NOFPG_LISTE.ajout_nom(nofpgl)
        nbpt = len(loc)
        FPG_LISTE.cree_oc(nom=nofpgl, long=nbpt)
        for kk in range(nbpt):
            FPG_LISTE.ecri_co(nom=nofpgl, indice=kk + 1, valeur=loc[kk])
        i += 1
        locIndex[nofpgl] = i

    timer.Stop('T6')
    timer.Start('T7')
    # dbgele can be set to True for an element
    dbgele = False
    k = 0
    ioptte = 0
    ielrefe = 0
    iflpg = 0
    for cata in cel.getElements():
        timer.Start('T7.1')
        k = k + 1
        l_elref1 = cata.elrefe
        l_decl_en = cata.nodes
        note = cata.name
        print "<I> On va traiter le TYPE_ELEM: " + note
        ERR.contexte("Examen du catalogue du type_elem : " + note)
        ERR.contexte("  rubrique: entete ", "AJOUT")
        if dbgele:
            print 'nodes:', [(i.name, i.nodes) for i in l_decl_en or []]

        NOMTE.ajout_nom(note)
        nute = NOMTE.jenonu(nom=note)
        if nute != k:
            ERR.mess('F', "bizarre !")
        note2 = txtpad(16, note)

        notm = cata.meshType.name
        nutm = NOMTM.jenonu(nom=notm)
        if nutm == 0:
            raise Exception('Erreur: nom de type_maille inconnu: %s' % notm)
        nno = NBNO.lit_co(nom=notm, indice=1)

        TYPEMA.ecri_os(indice=nute, valeur=cata.meshType.name)

        # objets NOELREFE et NBELREFE :
        # ---------------------------------
        kelrefe = 0
        for elref1 in cata.elrefe:
            kelrefe = kelrefe + 1
            ielrefe = ielrefe + 1
            NOELREFE.ecri_os(indice=ielrefe, valeur=elref1.elrefe.name)
        NBELREFE.ecri_os(indice=2 * (nute - 1) + 1, valeur=kelrefe)
        NBELREFE.ecri_os(
            indice=2 * (nute - 1) + 2, valeur=ielrefe - kelrefe + 1)

        # objets PNLOCFPG et NOLOCFPG :
        # ---------------------------------
        timer.Stop('T7.1')
        timer.Start('T7.2')
        num_elref1 = 0
        for elref1 in cata.elrefe:
            num_elref1 = num_elref1 + 1

            # familles de PG ordinaires
            for loca in elref1.gauss.keys():
                globa = elref1.gauss[loca]
                iflpg = iflpg + 1
                noflpg = note2 + \
                    txtpad(8, elref1.elrefe.name) + txtpad(8, loca)
                ifpg = NOFPG.jenonu(txtpad(8, elref1.elrefe.name) + globa)
                PNLOCFPG.ecri_os(indice=iflpg, valeur=noflpg)
                NOLOCFPG.ecri_os(indice=iflpg, valeur=ifpg)

            # famille de PG "MATER" (liste)
            if len(elref1.mater) > 0:
                if num_elref1 != 1:
                    ERR.mess(
                        'E', "Utilisation d'une famille de PG 'MATER' pour un elrefe non principal: " + elref1.elrefe.name)
                iflpg = iflpg + 1
                noflpg = note2 + \
                    txtpad(8, elref1.elrefe.name) + txtpad(8, 'MATER')
                # pour la famille de PG "MATER" : NOLOCFPG(iflpg)=0
                ifpg = 0
                PNLOCFPG.ecri_os(indice=iflpg, valeur=noflpg)
                NOLOCFPG.ecri_os(indice=iflpg, valeur=ifpg)

        # objet CTE_ATTR:
        # ---------------------------------
        liattr = get_liattr(cel, cata)
        nbattr = len(liattr)
        CTE_ATTR.cree_oc(nom=note, long=nbattr * 2)
        for iattr in range(nbattr):
            CTE_ATTR.ecri_co(nom=note, indice=2*iattr + 1, valeur=liattr[iattr][0].name)
            CTE_ATTR.ecri_co(nom=note, indice=2*iattr + 2, valeur=liattr[iattr][1])

        # modes locaux :
        # ---------------
        timer.Stop('T7.2')
        timer.Start('T7.3')
        modlocs = cata.usedLocatedComponents()
        ERR.contexte("Examen du catalogue du type_elem: " + note)
        ERR.contexte(
            "  rubrique: modes locaux utilises par le type_elem ", "AJOUT")

        # modes locaux "simples" :
        timer.Stop('T7.3')
        timer.Start('T7.4')
        for moloc in modlocs:
            if moloc.type not in ('ELEM', 'ELNO', 'ELGA'):
                continue
            nomolo = moloc.name
            assert nomolo != None, 'Il faut nommer explicitement tous les modes locaux crees dans les boucles.'
            nogd = moloc.physicalQuantity.name
            typept = moloc.type
            diff = moloc.diff
            nomolo2 = note2 + nomolo
            NOMMOLOC.ajout_nom(nomolo2)

            igd = NOMGD.jenonu(nogd)
            nec = DESCRIGD.lit_co(nom=nogd, indice=3)
            lcmp_gd = NOMCMP.objs[igd - 1].valeurs

            # calcul de nbpt, nbpt2, nbpt3 (et ifpg pour les familles ELGA):
            if typept == "ELEM":
                nbpt = 1
            if typept == "ELNO":
                nbpt = nno
            if typept == "ELGA":
                nbpt = -999
                nofpg1 = moloc.location
                # on cherche les caracteristiques de la famille nofpg1 : nbpt,ifpg
                # on ne regarde que l'elrefe principal :
                elref1 = l_elref1[0]

                # on cherche d'abord dans les familles "simples" :
                globa = elref1.gauss.get(nofpg1)
                if globa:
                    nbpt, ifpg = elrefe_npg(NOFPG, cata.elrefe[0].elrefe, globa)

                # si on n'a pas trouve, on regarde la famille "MATER" :
                if nbpt == -999:
                    if len(elref1.mater) > 0 and nofpg1 == 'MATER':
                        # on parcourt les familles "simples" de MATER :
                        nbpt_l = 0
                        for loca in elref1.mater:
                            globa = elref1.gauss[loca]
                            (nbpt, ifpg) = elrefe_npg(
                                NOFPG, cata.elrefe[0].elrefe, globa)
                            nbpt_l = nbpt_l + nbpt
                        nbpt = nbpt_l
                        ifpg = - locIndex[note2 + nofpg1]

            if nbpt == -999:
                ERR.mess(
                    'E', "Utilisation d'un nombre de points de Gauss indefini pour le mode_local: " + nomolo)
                assert 0

            if not diff:
                nbpt2 = nbpt
                nbpt3 = 1
            else:
                nbpt2 = nbpt + 10000
                nbpt3 = nbpt

            if typept == "ELGA":
                MODELOC.cree_oc(nom=nomolo2, long=4 + nec * nbpt3 + 1)
            else:
                MODELOC.cree_oc(nom=nomolo2, long=4 + nec * nbpt3)

            if typept == "ELEM":
                MODELOC.ecri_co(nom=nomolo2, indice=1, valeur=1)
            if typept == "ELNO":
                MODELOC.ecri_co(nom=nomolo2, indice=1, valeur=2)
            if typept == "ELGA":
                MODELOC.ecri_co(nom=nomolo2, indice=1, valeur=3)
            MODELOC.ecri_co(nom=nomolo2, indice=2, valeur=igd)
            MODELOC.ecri_co(nom=nomolo2, indice=4, valeur=nbpt2)
            if typept == "ELGA":
                MODELOC.ecri_co(
                    nom=nomolo2, indice=4 + nec * nbpt3 + 1, valeur=ifpg)
            if dbgele:
                print nomolo, typept, nogd, igd, diff, nbpt, nbpt2, nbpt3, nec

            if not diff:
                point = moloc.components
                liec = entiers_codes(note, point, lcmp_gd)
                for kk in range(len(liec)):
                    MODELOC.ecri_co(
                        nom=nomolo2, indice=4 + kk + 1, valeur=liec[kk])
                nbscal = len(point) * nbpt
                MODELOC.ecri_co(nom=nomolo2, indice=3, valeur=nbscal)
                if dbgele:
                    print nomolo, nbscal, 4 + kk + 1, liec
            else:
                nbscal = 0
                for xx in moloc.components:
                    en = xx[0]
                    point = xx[1:]
                    liec = entiers_codes(note, point, lcmp_gd)
                    liste = None
                    for decl_en in l_decl_en:
                        en2 = decl_en.name
                        liste2 = decl_en.nodes
                        if en2 == en:
                            liste = liste2
                    if dbgele:
                        print note, nomolo, point, lcmp_gd, liec, liste
                    if not liste:
                        # la verif. ci-dessous est trop sevère. Voir fiche REX
                        # 18068.
                        pass
                        # ERR.mess('E',"L' ensemble de noeuds "+en+" est
                        # non-defini pour l'element: "+note)
                    else:
                        for ino in liste:
                            for kk in range(len(liec)):
                                MODELOC.ecri_co(
                                    nom=nomolo2, indice=4 + (ino - 1) * nec + kk + 1, valeur=liec[kk])
                                if dbgele:
                                    print nomolo, 'ec=', liec[kk]
                            nbscal = nbscal + len(point)
                MODELOC.ecri_co(nom=nomolo2, indice=3, valeur=nbscal)
                if dbgele:
                    print nomolo, 'nbscal=', nbscal

        # modes locaux "vecteurs" :
        timer.Stop('T7.4')
        timer.Start('T7.5')
        for moloc in modlocs:
            if moloc.type not in ('VEC',):
                continue
            nomolo = moloc.name
            assert nomolo != None, 'Il faut nommer explicitement tous les modes locaux crees dans les boucles.'
            nogd = moloc.physicalQuantity.name
            molo1 = moloc.locatedComponents[0].name
            nomolo2 = note2 + nomolo
            igd = NOMGD.jenonu(nogd)
            NOMMOLOC.ajout_nom(nomolo2)
            MODELOC.cree_oc(nom=nomolo2, long=5)
            MODELOC.ecri_co(nom=nomolo2, indice=1, valeur=4)   # VECTEUR
            MODELOC.ecri_co(nom=nomolo2, indice=2, valeur=igd)
            nbscal = MODELOC.lit_co(nom=note2 + molo1, indice=3)
            MODELOC.ecri_co(nom=nomolo2, indice=3, valeur=nbscal)
            MODELOC.ecri_co(
                nom=nomolo2, indice=4, valeur=NOMMOLOC.jenonu(note2 + molo1))
            if dbgele:
                print nomolo, nogd, igd, nbscal, NOMMOLOC.jenonu(note2 + molo1)

        timer.Stop('T7.5')
        timer.Start('T7.6')
        # modes locaux "matrices" :
        for moloc in modlocs:
            if moloc.type not in ('MAT',):
                continue
            nomolo = moloc.name
            assert nomolo != None, 'Il faut nommer explicitement tous les modes locaux crees dans les boucles.'
            nogd = moloc.physicalQuantity.name
            molo1 = moloc.locatedComponents[0].name
            molo2 = moloc.locatedComponents[1].name
            nomolo2 = note2 + nomolo
            igd = NOMGD.jenonu(nogd)
            type_matrice = DESCRIGD.lit_co(nom=nogd, indice=1)
            NOMMOLOC.ajout_nom(nomolo2)
            MODELOC.cree_oc(nom=nomolo2, long=5)
            MODELOC.ecri_co(nom=nomolo2, indice=1, valeur=5)   # MATRICE
            MODELOC.ecri_co(nom=nomolo2, indice=2, valeur=igd)
            nbsca1 = MODELOC.lit_co(nom=note2 + molo1, indice=3)
            nbsca2 = MODELOC.lit_co(nom=note2 + molo2, indice=3)
            if molo2 != molo1 and type_matrice != 5:
                raise Exception("Erreur")
            if type_matrice == 4:
                nbscal = nbsca1 * (nbsca1 + 1) / 2
            elif type_matrice == 5:
                nbscal = nbsca1 * nbsca2
            else:
                raise Exception("Erreur")
            MODELOC.ecri_co(nom=nomolo2, indice=3, valeur=nbscal)
            MODELOC.ecri_co(
                nom=nomolo2, indice=4, valeur=NOMMOLOC.jenonu(note2 + molo1))
            MODELOC.ecri_co(
                nom=nomolo2, indice=5, valeur=NOMMOLOC.jenonu(note2 + molo2))
            if dbgele:
                print nomolo, nogd, igd, nbscal, NOMMOLOC.jenonu(note2 + molo1), NOMMOLOC.jenonu(note2 + molo2)

        timer.Stop('T7.6')
        timer.Start('T7.7')
        # options :
        # ---------------
        dico_opt_te = {}
        for noop, opt in cata.getCalculs():
            numte = opt.te
            nbin = len(opt.para_in)
            nbou = len(opt.para_out)
            ERR.contexte("Examen du catalogue du type_elem : " + note)
            ERR.contexte("  rubrique: option : " + noop, "AJOUT")
            ERR.veri_pas_doublon_lpara('E', opt.para_in)
            ERR.veri_pas_doublon_lpara('E', opt.para_out)

            if dico_opt_te.get(noop):
                ERR.mess(
                    'E', "L'option: " + noop + " est definie plusieurs fois pour le TYPE_ELEM: " + note)
            else:
                dico_opt_te[noop] = numte

            if numte < 0:
                assert numte == -1 or numte == -2, (note, noop, numte)
                nbin = 0
                nbout = 0

            ioptte = ioptte + 1
            nuop = NOMOP.jenonu(nom=noop)
            OPTT2.ecri_os(indice=2 * (ioptte - 1) + 1, valeur=nuop)
            OPTT2.ecri_os(indice=2 * (ioptte - 1) + 2, valeur=nute)
            OPTMOD.cree_oc(nom=str(ioptte), long=3 + nbin + nbou)
            OPTNOM.cree_oc(nom=str(ioptte), long=nbin + nbou)
            OPTMOD.ecri_co(nom=str(ioptte), indice=1, valeur=numte)
            OPTMOD.ecri_co(nom=str(ioptte), indice=2, valeur=nbin)
            OPTMOD.ecri_co(nom=str(ioptte), indice=3, valeur=nbou)
            if dbgele:
                print noop, nuop, nute, numte, nbin, nbou

            if numte > 0:
                for kk in range(nbin):
                    param = opt.para_in[kk][0].name
                    mode = opt.para_in[kk][1].name
                    OPTNOM.ecri_co(
                        nom=str(ioptte), indice=kk + 1, valeur=param)
                    OPTMOD.ecri_co(
                        nom=str(ioptte), indice=3 + kk + 1, valeur=NOMMOLOC.jenonu(note2 + mode))
                    if dbgele:
                        print noop, 'in', param, mode, kk + 1, 3 + kk + 1, NOMMOLOC.jenonu(note2 + mode)

                for kk in range(nbou):
                    param = opt.para_out[kk][0].name
                    mode = opt.para_out[kk][1].name
                    OPTNOM.ecri_co(
                        nom=str(ioptte), indice=nbin + kk + 1, valeur=param)
                    OPTMOD.ecri_co(
                        nom=str(ioptte), indice=3 + nbin + kk + 1, valeur=NOMMOLOC.jenonu(note2 + mode))
                    if dbgele:
                        print noop, 'out', param, mode, nbin + kk + 1, 3 + nbin + kk + 1, NOMMOLOC.jenonu(note2 + mode)

        timer.Stop('T7.7')
        timer.Start('T7.8')
        # -- on emet une erreur si le type_elem calcule des options qu'il NE DEVRAIT PAS calculer
        ERR.contexte("Examen du catalogue du type_elem : " + note)
        for noop in dico_opt_te.keys():
            #           -- si numte=-1, ce n'est pas tres grave pour l'instant :
            if dico_opt_te[noop] == -1:
                continue

            if noop in opt_contrainte:
                if noop not in opt_a_calculer[note]:
                    ERR.mess(
                        'E', "L'option: " + noop + " NE DOIT PAS etre calculee par le TYPE_ELEM: " + note)
                    raise

        timer.Stop('T7.8')
        timer.Start('T7.9')
        # -- on ajoute des "-1" pour les options que le type_elem DEVRAIT calculer
        #    et qu'il ne calcule pas.
        for noop in opt_a_calculer[note]:
            if not noop in dico_opt_te:
                numte = -1
                nbin = 0
                nbout = 0
                ioptte = ioptte + 1
                nuop = NOMOP.jenonu(nom=noop)
                OPTT2.ecri_os(indice=2 * (ioptte - 1) + 1, valeur=nuop)
                OPTT2.ecri_os(indice=2 * (ioptte - 1) + 2, valeur=nute)
                OPTMOD.cree_oc(nom=str(ioptte), long=3 + nbin + nbou)
                OPTNOM.cree_oc(nom=str(ioptte), long=nbin + nbou)
                OPTMOD.ecri_co(nom=str(ioptte), indice=1, valeur=numte)
                OPTMOD.ecri_co(nom=str(ioptte), indice=2, valeur=nbin)
                OPTMOD.ecri_co(nom=str(ioptte), indice=3, valeur=nbou)

        timer.Stop('T7.9')
        del cata

    assert(ioptte == nbopte)
    timer.Stop('T7')
    timer.Start('T8')

    #  catalogue des PHENOMENE_MODELISATION :
    #--------------------------------------------------------
    ERR.contexte("Examen du catalogue phenomene modelisation : ")
    PHENOMENE = JV.cree_pn(d, nom='&CATA.PHENOMENE', tsca='K16')

    for pheno in cel.getPhenomenons():
        ph = pheno.name
        codph = pheno.code
        lmod = pheno.modelisations
        PHENOMENE.ajout_nom(ph)
        MODELI = JV.cree_co(d, nom='&CATA.' + ph, tsca='I',
                            tsca_pn='K16', contig='CONTIG', acces='NU', longv=(nbtm + 2))
        NOMMODELI = JV.cree_pn(
            d, nom='&CATA.' + txtpad(13, ph) + '.MODL', tsca='K16')
        for mod in lmod.keys():
            modeli = lmod[mod]
            codmod = modeli.code
            (d1, d2) = modeli.dim
            laffe = modeli.elements
            lattrib = modeli.attrs
            NOMMODELI.ajout_nom(mod)
            MODELI.cree_oc(nom=mod, long=(nbtm + 2))
            MODELI.ecri_co(nom=mod, indice=nbtm + 1, valeur=int(d1))
            MODELI.ecri_co(nom=mod, indice=nbtm + 2, valeur=int(d2))
            for (tyma, tyel) in laffe:
                MODELI.ecri_co(
                    nom=mod, indice=NOMTM.jenonu(nom=tyma.name), valeur=NOMTE.jenonu(nom=tyel.name))

    timer.Stop('T8')
    timer.Start('T9')
    #  impression des obj :
    #-----------------------------------------
    likeys = d.keys()
    likeys.sort()
    for nomojb in likeys:
        ojb = d[nomojb]
        ojb.impr(file)
        if dbgdir:
            fdbg = osp.join(dbgdir, nomojb.replace(' ', '_') + '.ojb')
            ojb.impr(open(fdbg, 'wb'))
    ERR.mess(
        'I', "Fin de la transformation de l'ENSEMBLE des catalogues en objets jeveux")
    timer.Stop('T9')
    print timer

#---------------------------------------------------------------------------
# TODO should be CataElem.getAttrsElement(element)
_cache_attr = {}
def get_liattr(cel, cata):
    #     retourne la liste des attributs d'un type_elem :
    #     (y compris les attributs definis au niveau des modelisations)
    #     (y compris les attributs definis AUTOMATIQUEMENT)
    #-------------------------------------------------------------------------
    note = cata.name
    if _cache_attr.get(note):
        return _cache_attr[note]
    tyma1 = cata.meshType

    # recherche d'informations sur le type de maille : codtma (K3) + dimension
    # topologique
    dimtma = tyma1.dim
    codtma = tyma1.code

    dicattr = {}

    # Attributs definis pour toute la modelisation :
    # Remarque : pour les type_elem appartenant à plusieurs modelisations,
    #    si un attribut doit avoir plusieurs valeurs differentes, on lui
    # affecte la valeur "###" (qui veut dire plusieurs) ou (-999 si c'est un entier)
    #    Si c'est embetant, il faut redefinir l'attribut au niveau du type_elem

    lattr_AUTO = [AT.ALIAS8, AT.PHENO, AT.MODELI, AT.DIM_TOPO_MODELI,
                  AT.DIM_COOR_MODELI, AT.DIM_TOPO_MAILLE, AT.BORD, AT.DISCRET]

    for pheno, modeli in cel.getElemModel(cata.name):
            codph = pheno.code
            codmod = modeli.code
            (d1, d2) = modeli.dim
            laffe = modeli.elements
            lattrib = modeli.attrs
            d1 = int(d1)
            d2 = int(d2)
            assert d1 in (-1, 0, 1, 2, 3), d1
            assert d2 in (0, 1, 2, 3), d2
            # On ajoute les attributs definis AUTOMATIQUEMENT (ceux de
            # lattr_AUTO) :

            # Si les attributs automatiques existent deja, c'est que l'element est partage.
            # On verifie alors la coherence des informations
            if dicattr.get(AT.ALIAS8) is None:
                dicattr[AT.DIM_TOPO_MAILLE] = str(dimtma)
                dicattr[AT.DIM_TOPO_MODELI] = str(d1)
                dicattr[AT.DIM_COOR_MODELI] = str(d2)
                dicattr[AT.ALIAS8] = str(codph)[0:2] + str(codmod)[0:3] + str(codtma)[0:3]
                dicattr[AT.PHENO] = str(codph)[0:2]
                dicattr[AT.MODELI] = str(codmod)[0:3]

            else:
                if dicattr[AT.DIM_TOPO_MAILLE] != str(dimtma):
                    ERR.mess('E', "DIM_TOPO_MAILLE mal defini (plusieurs)")
                if dicattr[AT.DIM_TOPO_MODELI] != str(d1):
                    ERR.mess('E', "DIM_TOPO_MODELI mal defini (plusieurs)")
                if dicattr[AT.DIM_COOR_MODELI] != str(d2):
                    ERR.mess('E', "DIM_COOR_MODELI mal defini (plusieurs)")
                if dicattr[AT.ALIAS8][5:] != str(codtma)[0:3]:
                    ERR.mess(
                        'E', "code type_maille mal defini (plusieurs)")

                alias8 = dicattr[AT.ALIAS8]
                if alias8[:2] != str(codph)[0:2]:
                    dicattr[AT.PHENO] = '##'
                    alias8 = '##' + alias8[2:]
                if alias8[2:5] != str(codmod)[0:3]:
                    dicattr[AT.MODELI] = '###'
                    alias8 = alias8[:2] + '###' + alias8[5:]
                dicattr[AT.ALIAS8] = alias8

            # le cas d1 == -1 est particulier : il est reserve aux
            # modelisations discrètes DIS_xxx
            if d1 == -1:
                dicattr[AT.DISCRET] = 'OUI'
                dicattr[AT.PRINCIPAL] = 'OUI'
                dicattr[AT.BORD] = '0'
            else:
                dicattr[AT.DISCRET] = 'NON'
                if d1 > d2:
                    ERR.mess(
                        'E', "Pb. pour les dimensions  d1 d2 de la modelisation:" + mod)

                if dimtma == d1:
                    dicattr[AT.PRINCIPAL] = 'OUI'
                    dicattr[AT.BORD] = '0'
                else:
                    if dimtma == d1 - 1:
                        dicattr[AT.BORD] = '-1'
                    elif dimtma == d1 - 2:
                        dicattr[AT.BORD] = '-2'
                    elif dimtma == d1 - 3:
                        dicattr[AT.BORD] = '-3'
                    elif dimtma == d1 + 1:
                        dicattr[AT.BORD] = '+1'
                    else:
                        assert False, (mod, d1, dimtma)

            for attr, val_attr in modeli.attrs or []:
                if attr in lattr_AUTO:
                    ERR.mess(
                        'E', "Il est interdit de redefinir l'attribut:" + attr.name)
                dicattr[attr] = val_attr

    # surcharge eventuelle des attributs definis pour le type_elem:
    for attr, val_attr in cata.getAttrs() or []:
        # XXX was "and" !
        if attr in lattr_AUTO or attr in dicattr:
            ERR.mess(
                'E', "Il est interdit de redefinir l'attribut:" + attr.name)
        dicattr[attr] = val_attr

    liattr = dicattr.items()
    _cache_attr[note] = liattr
    return liattr


#-------------------------------------------------------------------------
def liste_opt_a_calculer(cel, dbgdir=None):
    #   -- retourne 2 dictionnaires :
    #      opt_contrainte[noop]=1
    #        => noop est une option "contrainte" (il existe un bloc COND_CALCUL dans son catalogue.)
    #      opt_a_calculer[nomte]=[noop1,noop2,...]
    #        => liste des options contraintes qui DOIVENT etre calculees par nomte
    #   ----------------------------------------------------------------------
    opt_contrainte = {}
    opt_a_calculer = {}

#   -- On calcule :
#      dic1[attr=valattr]=set([nomte])
#   ------------------------------------
    dic1 = {}
    for cata in cel.getElements():
       # entete, modlocs, opts = cata.cata_te
        nomte = cata.name
        opt_a_calculer[nomte] = []
        liattr = get_liattr(cel, cata)
        for attr, val in liattr:
            dic1[(attr, val)] = dic1.get((attr, val), set())
            dic1[(attr, val)].add(nomte)

#   -- On remplit opt_a_calculer :
#   -------------------------------
    for cata in cel.getOptions():
      # noop, lpain, lchou, lpaou, cond_calcul = cata.cata_op
        noop = cata.name
        cond_calcul = cata.condition
        if not cond_calcul:
            continue

        opt_contrainte[noop] = 1
        set_total = set()
        for cond_calc in cond_calcul:
            cond1 = cond_calc.conditions[0]
            set1 = set(dic1[cond1])
            for cond1 in cond_calc.conditions[:]:
                set1.intersection_update(dic1[cond1])

            if cond_calc.addCondition():
                set_total.update(set1)
            else:
                set_total.difference_update(set1)
        l1 = list(set_total)
        for nomte in l1:
            opt_a_calculer[nomte].append(noop)

    if dbgdir:
        opt_contrainte = OrderedDict(sorted(opt_contrainte.items(),
                                            key=lambda i: i[0]))
        fdbg = open(osp.join(dbgdir, 'opt_contrainte'), 'wb')
        fdbg.write(os.linesep.join(opt_contrainte.keys()))
        fdbg.close()
        fdbg = open(osp.join(dbgdir, 'opt_a_calculer'), 'wb')
        for nomte in sorted(opt_a_calculer.keys()):
            fdbg.write(' '.join([nomte, ":"] + sorted(opt_a_calculer[nomte])))
            fdbg.write(os.linesep)
            opt_a_calculer[nomte] = sorted(opt_a_calculer[nomte])
        opt_a_calculer = OrderedDict(sorted(opt_a_calculer.items(),
                                            key=lambda i: i[0]))
        fdbg.close()
    return (opt_contrainte, opt_a_calculer)


def calc_nbopte(cel, opt_a_calculer):
    # retourne nbopte : le nombre total de couples (nomte,noop) ayant un sens

    nbopte = 0
    for cata in cel.getElements():
       # entete, modlocs, opts = cata.cata_te
        note = cata.name
        dico_opt_te = {}

#       -- 1. les couples declares dans les catalogues de type_elem :
        for noop, opt in cata.getCalculs():
            dico_opt_te[noop] = 1
            nbopte = nbopte + 1

#       -- 2. les couples absents (pas encore programmes => -1) :
        for noop in opt_a_calculer[note]:
            if not dico_opt_te.get(noop):
                nbopte = nbopte + 1
    return nbopte

# TODO CataElem.allLocations()
def getAllLocations(cel):
    #  retourne un dictionnaire contenant toutes les definitions des familles "liste" de PG
    #-------------------------------------------------------------------------
    lifpgl = {}
    for cata in cel.getElements():
        note2 = txtpad(16, cata.name)
        for elref1 in cata.elrefe:
            if len(elref1.mater) > 0:
                nofpgl = "MATER"
                lifpgl[note2 + nofpgl] = copy.deepcopy(elref1.mater)
                lifpgl[note2 + nofpgl].append(elref1.elrefe.name)
    locations = OrderedDict(sorted(lifpgl.items(), key=lambda i: i[0]))
    return locations



#============================================================================================
# Quelques fonctions utiles pour engendrer des fichiers pratiques pour les programmeurs
# (voir la variable xxut1 ci-dessus)
#=========================================================================

#-------------------------------------------------------------------------


def impr_CMP(nomfic, cel):
    # pour imprimer tous les 6-uplets ( OPTION  TYPELEM  IN/OUT  PARAM  GRANDEUR  CMP )
    #-------------------------------------------------------------------------
    file = open(nomfic, "w")

    for cata in cel.getElements():
        note = cata.name
        modlocs = cata.usedLocatedComponents()

        dicmod = {}
        for moloc in modlocs:
            if moloc.type not in ('ELEM', 'ELNO', 'ELGA'):
                continue
            nomolo = moloc.name
            nogd = moloc.physicalQuantity.name
            typept = moloc.type
            diff = moloc.diff

            licmp = []
            if not diff:
                point = moloc.components
                licmp.extend(point)
            else:
                for xx in moloc.components:
                    en = xx[0]
                    point = xx[1:]
                    licmp.extend(point)
            licmp = list(set(licmp))
            licmp.sort()
            dicmod[nomolo] = (nogd, licmp)

        for noop, opt in cata.getCalculs():
            numte = opt.te
            nbin = len(opt.para_in)
            nbou = len(opt.para_out)

            if numte > 0:

                for kk in range(nbin):
                    param = opt.para_in[kk][0].name
                    mode = opt.para_in[kk][1].name
                    if mode in dicmod:
                        nogd, licmp = dicmod[mode]
                        for cmp in licmp:
                            file.write(
                                noop + " " + note + " IN " + param + " " + nogd + " " + cmp + "\n")

                for kk in range(nbou):
                    param = opt.para_out[kk][0].name
                    mode = opt.para_out[kk][1].name
                    if mode in dicmod:
                        nogd, licmp = dicmod[mode]
                        for cmp in licmp:
                            file.write(
                                noop + " " + note + " OUT " + param + " " + nogd + " " + cmp + "\n")


#-------------------------------------------------------------------------
def impr_param_options(nomfic, cel):
    # pour imprimer tous les 5-uplets ( OPTION  TYPELEM  IN/OUT  PARAM  GRANDEUR)  (y compris les RESL)
    #-------------------------------------------------------------------------
    file = open(nomfic, "w")

    for cata in cel.getElements():
        note = cata.name
        modlocs = cata.usedLocatedComponents()
        dicmod = {}
        for moloc in modlocs:
            nomolo = moloc.name
            assert nomolo != None, 'Il faut nommer explicitement tous les modes locaux crees dans les boucles.'
            nogd = moloc.physicalQuantity.name
            dicmod[nomolo] = nogd

        for noop, opt in cata.getCalculs():
            numte = opt.te
            nbin = len(opt.para_in)
            nbou = len(opt.para_out)

            if numte > 0:

                for kk in range(nbin):
                    param = opt.para_in[kk][0].name
                    mode = opt.para_in[kk][1].name
                    if mode in dicmod:
                        nogd = dicmod[mode]
                        file.write(
                            noop + " " + note + " IN " + param + " " + nogd + "\n")

                for kk in range(nbou):
                    param = opt.para_out[kk][0].name
                    mode = opt.para_out[kk][1].name
                    if mode in dicmod:
                        nogd = dicmod[mode]
                        file.write(
                            noop + " " + note + " OUT " + param + " " + nogd + "\n")


#-------------------------------------------------------------------------
def PbOptions(cel):
    # pour imprimer les noms des options qui ne sont plus realisees
    # pour imprimer les noms des parametres inutilises des options
    #-------------------------------------------------------------------------
    # La fonction n'est pas a jour ... Il faut la corriger !
    utilise = {}
    for cata in cel.getElements():
        entete, modlocs, opts = cata.cata_te
        note = entete[0]

        if opts:
            for opt in opts:
                noop = opt[0]
                numte = opt[1]
                nbin = len(opt[2]) / 2
                nbou = len(opt[3]) / 2
                if not noop in utilise:
                    utilise[noop] = []
                if numte > 0:
                    for kk in range(nbin):
                        param = opt[2][2 * kk + 1]
                        utilise[noop].append(param)
                    for kk in range(nbou):
                        param = opt[3][2 * kk + 1]
                        utilise[noop].append(param)

    declare = {}
    for cata in cel.op:
        noop, lchin, lchou, comlibr, cond_calcul = cata.cata_op
        declare[noop] = []
        for (param, nogd, localis, comlibr) in lchin:
            declare[noop].append(param)
        for (param, nogd, localis, comlibr) in lchou:
            declare[noop].append(param)

    # les parametres declares et non utilises sont a supprimer :
    lopt = declare.keys()
    lopt.sort()
    erreur = False
    for noop in lopt:
        if not noop in utilise:
            ERR.mess(
                'E', "L'option " + noop + " n'est calculee par aucun element.")

            erreur = True
            continue
        for param in declare[noop]:
            if not param in utilise[noop]:
                # exception acceptee :
                if noop == 'SIRO_ELEM' and param == 'XXXXXX':
                    continue
                ERR.mess(
                    'E', "Le parametre " + param + " de l'option " + noop + " n'est utilise par aucun element.")
                erreur = True


#-------------------------------------------------------------------------
def numte_lnomte(nomfic, cel):
    # pour imprimer les noms des type_elem qui utilisent une routine te00ij
    #-------------------------------------------------------------------------
    file = open(nomfic, "w")
    dico = {}
    for cata in cel.getElements():
        note = cata.name
        for noop, opt in cata.getCalculs():
            numte = opt.te
            if numte > 0 and numte != 99:
                numte = 1000 + numte
                numte = 'te0' + str(numte)[1:]
                if not numte in dico:
                    dico[numte] = []
                dico[numte].append(note)
    l1 = dico.keys()
    l1.sort()
    for numte in l1:
        file.write(numte + ' ')
        for note in dico[numte]:
            file.write(note + ' ')
        file.write('\n')


#-------------------------------------------------------------------------
def nomte_nomtm(nomfic, cel):
    # pour imprimer les lignes (type_elem, type_maille, attribut1, attribut2, ... )
    #-------------------------------------------------------------------------
    file = open(nomfic, "w")
    lines = []
    for cata in cel.getElements():
        note = cata.name
        notm = cata.meshType.name
        liattr = get_liattr(cel, cata)
        # n1 = len(liattr)
        # assert 2 * (n1 / 2) == n1, n1
        # l1 = " "
        line = " ".join(["{}={}".format(attr.name, val) for attr, val in liattr])
        lines.append("{:16} {:8} {:17}".format(note, notm, line))
        # for k in range(n1 / 2):
        #     x1 = "%-17s" % (liattr[2 * k] + "=" + liattr[2 * k + 1],)
        #     l1 = l1 + x1 + " "
    lines.append('')
    file.write('\n'.join(lines))


#-------------------------------------------------------------------------
def verif_phenmode(cel):
    # verifie que les elements se retrouvent dans PHENOMENE_MODELISATION :
    #-------------------------------------------------------------------------
    dic1 = {}
    for cata in cel.getElements():
        note = cata.name
        dic1[note] = 1

    dic2 = {}
    for pheno in cel.getPhenomenons():
        ph = pheno.name
        lmod = pheno.modelisations
        for mod in lmod.keys():
            modeli = lmod[mod]
            laffe = modeli.elements
            for (tyma, tyel) in laffe:
                dic2[tyel.name] = 1

    s1 = set(dic1.keys())
    s2 = set(dic2.keys())

    s3 = s1.difference(s2)
    for tyel in s3:
        if tyel[0:8] in ('D_DEPL_R', 'D_TEMP_R', 'D_PRES_C'):
            continue
        ERR.mess('E', "L'element " + tyel +
                 " doit figurer dans la catalogue PHENOMENE_MODELISATION__ .")

    s3 = s2.difference(s1)
    for tyel in s3:
        ERR.mess('E', "L'element " + tyel +
                 " n'est pas decrit dans un catalogue d'element .")
