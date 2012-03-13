#@ MODIF imprime Lecture_Cata_Ele  DATE 13/03/2012   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
# RESPONSABLE VABHHTS J.PELLET
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

# --------------------------------------------------------------------------------
#       impressions du catalogue à différents formats :   cata /cata_l/ ojb
# --------------------------------------------------------------------------------
import string,copy,os ,traceback
from  Lecture_Cata_Ele import utilit
ut=utilit
ERR=ut.ERR
# --------------------------------------------------------------------------------
# Remarque : Ce fichier contient des "bouts" de code que l'on peut facilement executer
#            pour obtenir des fichiers de documentation ou de problemes.
#            Ces bouts de code se situent ci-dessous vers la chaine "XXUTIL"
# --------------------------------------------------------------------------------


#######################################################################################################
# Fonction principale :
#######################################################################################################

def impr_cata(capy,nomfic,format,seq='oui'):
#==========================================


#   imprimer un catalogue python (capy) sur un fichier à un format donné
#       seq='non' permet de demander le "split" en autant de fichiers qu'il y a de catalogues
#       format='ojb'     : impression des objets jeveux correspondant à la SD &CATA
#       format='cata'    : impression au format ".cata" (en texte)
#       format='cata_l'  : impression au format ".cata" (en texte) avec des lignes "longues" (plus facile pour les grep)

        if seq=="oui":
            fimpr = open(nomfic,"w")
        elif seq=="non":
            fimpr=nomfic
            try:
                os.mkdir(nomfic)
            except : pass
        else:
            raise Exception("Erreur")

        if format == 'cata' :
            ut.menage_capy(capy)
            imprime_cata(fimpr,capy,seq,format)
        elif format == 'cata_l' :
            ut.menage_capy(capy)
            imprime_cata(fimpr,capy,seq,format)
        elif format == 'ojb' :
            imprime_ojb(fimpr,capy)
        else :
            raise Exception("Erreur_Fatale")
        if seq=="oui": fimpr.close()
        ERR.fini()


#######################################################################################################
# utilitaires :
#######################################################################################################


def txtpad(long,chaine):
#---------------------------------------
#   retourne une chaine de longueur "long" en complétant chaine par des "blancs"
    if len(chaine) > long : return chaine[0:long]
    chaine2=chaine+" "*(long-len(chaine))
    return chaine2


#######################################################################################################
# sous_fonctions :
#######################################################################################################


def imprime_cata(file2,capy,seq,format):
#---------------------------------------

       if seq=="oui":
           file=file2

   #  impression du catalogue des GRANDEUR :
   #-----------------------------------------
       cata=capy.gd
       if cata :
           if seq=="non": file = open(file2+"/grandeur_simple__.cata","w")
           file.write( cata.cmodif+"\n")
           imprime_copyright(file)
           file.write( "GRANDEUR_SIMPLE__"+"\n")
           for gd in cata.l_gdsimp :
               if   gd.comlibr : file.write( gd.comlibr + "\n")
               if  not gd.union :
                  file.write( "    %-8s = %-3s  " % (gd.nom,gd.tscal))
                  imprime_lk8(format,file,gd.lcmp,8,decal1=0,decaln=20)
               else :
                  file.write( "    %-8s = UNION__  " % (gd.nom))
                  imprime_lk8(format,file,gd.lgd,8,decal1=0,decaln=20)

           file.write( "\nGRANDEUR_ELEMENTAIRE__"+"\n")
           for gd in cata.l_gdelem :
               if len(gd.gdelem)==3 :
                   file.write( "   "+gd.nom+ " 2 ")
                   imprime_lk8(format,file,gd.gdelem,3)
               if len(gd.gdelem)==1 :
                   file.write( "   "+gd.nom+" 1 ")
                   imprime_lk8(format,file,gd.gdelem,1)
           file.write( "\n")


   #  impression du catalogue des TYPE_MAILLE__ :
   #-----------------------------------------
       cata=capy.tm  ; dico_bidon={};
       if cata :
           if seq=="non": file = open(file2+"/type_maille__.cata","w")
           file.write( cata.cmodif+"\n")
           imprime_copyright(file)
           file.write("TYPE_MAILLE__\n")
           for k in range(len(cata.ltm)):
               catak=cata.ltm[k]
               file.write( "\n\nMAILLE__ %-8s   %-5s   DIM__  %-1s   CODE__  %-5s" %  (catak[0],str(catak[1]), catak[2],catak[3]) )
               for elrefe in cata.ltm[k][4]:
                  file.write( "\n   ELREFE__ %-8s  " % (elrefe[0]) )
                  for fampg in elrefe[1] :
                     file.write( "\n         FAMILLE__ %-8s    %-3s" % (fampg[0],str(fampg[1])) )


   #  impression du catalogue des PHENOMENE_MODELISATION__ :
   #--------------------------------------------------------
       cata=capy.ph
       if cata :
           if seq=="non": file = open(file2+"/phenomene_modelisation__.cata","w")
           file.write( cata.cmodif+"\n")
           imprime_copyright(file)
           file.write( "PHENOMENE_MODELISATION__  " +"\n")
           for (ph,lmod,codph) in cata.l_pheno:
               file.write( "\n   PHENOMENE__  "+ph+"       CODE__  "+codph+"\n")
               for (mod,laffe,codmod,(d1,d2),lattrib) in lmod:
                   file.write( "\n       MODELISATION__ %-16s   DIM__ %-1s %-1s   CODE__ %s\n" % (mod,d1,d2,codmod) )
                   if (lattrib) :
                       file.write( "              ATTRIBUT__",)
                       for (x,y) in lattrib :  file.write( "  %s=%s" % (x,y))
                       file.write( "\n",)
                   for (tyma,tyel) in laffe:
                       file.write( "              MAILLE__ %-8s  ELEMENT__ %-16s\n" %(tyma,tyel))
           file.write( "\n")


   #  impression des catalogues des OPTION__ :
   #-----------------------------------------
       for cata in capy.op:
           nom,lchin,lchou,comlibr=cata.cata_op
           if seq=="non": file = open(file2+"/"+string.lower(nom)+".cata","w")
           file.write( cata.cmodif +"\n")
           imprime_copyright(file)
           file.write( nom +"\n")
           if comlibr : file.write( comlibr +"\n")
           file.write( "OPTION__\n")
           file.write( "  IN__  " +"\n")
           for (para,nogd,localis,comlibr) in lchin :
              if not comlibr : comlibr=" "
              if not localis: localis=" "
              file.write( "    %-10s %-10s   %s %s\n" %(para,nogd,localis,comlibr))
           file.write( "   OUT__ " +"\n")
           for (para,nogd,typout,comlibr) in lchou :
              if not comlibr : comlibr=" "
              file.write( "    %-10s %-10s %-6s   %s\n" %(para,nogd,typout,comlibr))
           file.write( "\n")
           if seq=="non":file.close()


   #  impression des catalogues des TYPE_GENE :
   #-------------------------------------------
       for cata in capy.tg:
           nom,l_entete,modlocs,opts=cata.cata_tg
           if seq=="non": file = open(file2+"/"+string.lower(nom)+".cata","w")
           file.write( cata.cmodif +"\n")
           imprime_copyright(file)
           file.write( nom +"\n")
           file.write( "TYPE_GENE__\n")


           for entete in l_entete:
               l_elref1=entete[2];lattrib=entete[3];l_decl_en=entete[4];l_decl_opt=entete[5]
               impr_entete(file,entete,l_elref1,lattrib,l_decl_en,l_decl_opt,format)

           #impression des modes locaux et des options
           impr_moloc_opt(file,modlocs,opts,format)
           if seq=="non":file.close()


   #  impression des catalogues des TYPE_ELEM__ :
   #-------------------------------------------
       for cata in capy.te:
           entete,modlocs,opts=cata.cata_te
           if seq=="non": file = open(file2+"/"+string.lower(entete[0])+".cata","w")
           l_elref1=entete[2];lattrib=entete[3];l_decl_en=entete[4]
           file.write( cata.cmodif +"\n")
           imprime_copyright(file)
           file.write( entete[0]+"\n\n")
           file.write( "\nTYPE_ELEM__ ")
           impr_entete(file,entete,l_elref1,lattrib,l_decl_en,None,format)

           #impression des modes locaux et des options
           impr_moloc_opt(file,modlocs,opts,format)
           if seq=="non":file.close()



def imprime_copyright(file):
#------------------------------------------------------------
#  impression des 17 lignes de copyright EDF sur le fichier file
#------------------------------------------------------------
   file.write('%            CONFIGURATION MANAGEMENT OF EDF VERSION                        '+"\n")
   file.write('% ======================================================================    '+"\n")
   # pour déjouer un bug de l'agla, on coupe la ligne suivante en 2 :
   file.write('% COPY')
   file.write(      'RIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG    '+"\n")
   file.write('% THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY      '+"\n")
   file.write('% IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY      '+"\n")
   file.write('% THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR         '+"\n")
   file.write('% (AT YOUR OPTION) ANY LATER VERSION.                                       '+"\n")
   file.write('%                                                                           '+"\n")
   file.write('% THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT       '+"\n")
   file.write('% WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF                '+"\n")
   file.write('% MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU          '+"\n")
   file.write('% GENERAL PUBLIC LICENSE FOR MORE DETAILS.                                  '+"\n")
   file.write('%                                                                           '+"\n")
   file.write('% YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE         '+"\n")
   file.write('% ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,             '+"\n")
   file.write('%    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.            '+"\n")
   file.write('% ======================================================================    '+"\n")



def imprime_lk8(format,file,liste,ncol,decal1=0,decaln=0,retour=1):
#------------------------------------------------------------
#  impression d'une liste de K8 :
#    format : si format='cata_l' : on ecrit tout sur la meme ligne
#             sinon : on tient compte de l'argument nbcol
#    ncol   : nombre de colonnes voulu (après, on revient à la ligne)
#    decal1 : nombre de " " imprimé au début de la 1ere ligne.
#    decaln : nombre de " " imprimé au début des lignes suivantes
#    retour : 1 -> on termine par un \n ; 0 -> on n'écrit pas de \n
#------------------------------------------------------------
    if format == 'cata_l' :
      nbcol= len(liste)
    else :
      nbcol= ncol
    if nbcol <= 0 : nbcol=1

    nblig=len(liste)/nbcol +1
    reste = len(liste) % nbcol

    ilig = 0
    for k in range(nblig-1):
        ilig = ilig +1
        if ilig == 1 : file.write(" "*decal1)
        else :  file.write(" "*decaln)
        for j in range(nbcol) : file.write("%-8s " % liste [k*nbcol +j])
        if ilig < nblig - 1  : file.write("\n")
        if ilig == nblig - 1 and reste > 0 : file.write("\n")
    if reste == 0  and retour : file.write("\n")
    if reste > 0 :
        ilig = ilig +1
        if ilig == 1 : file.write(" "*decal1)
        else :  file.write(" "*decaln)
        for j in range(len(liste) % nbcol) : file.write("%-8s " % liste [(nblig-1)*nbcol +j])
        if retour : file.write("\n")


def tri_moloc(m1,m2):
# pour trier les modes_locaux selon le nom de la grandeur :
   return  cmp(m1[1],m2[1])




def impr_moloc_opt(file,modlocs,opts,format):
#-------------------------------------------------------------------------------------
#  impression des modes locaux et des options  pour un TYPE_ELEM__ ou 1 TYPE_GENE__
#--------------------------------------------------------------------------------------
    MLOCs,MLVEs,MLMAs=modlocs

    file.write( "\n\nMODE_LOCAL__ " +"\n")
    MLOCs.sort(tri_moloc)
    for moloc in MLOCs:
        if moloc[2]=="ELEM__" :
            file.write("    %-8s = %-8s %-6s        "  % moloc[0:3])
            point=moloc[5]
            file.write(" (");imprime_lk8(format,file,point,6,decal1=0,decaln=40,retour=0);file.write(")\n")
        elif moloc[2]=="ELGA__" :
            file.write("    %-8s = %-8s %-6s %-6s "  % moloc[0:4])
            point=moloc[5]
            file.write(" (");imprime_lk8(format,file,point,6,decal1=0,decaln=40,retour=0);file.write(")\n")
        elif moloc[2]=="ELNO__" :
            file.write("    %-8s = %-8s %-6s %-4s__ "  % (moloc[0],moloc[1],moloc[2],moloc[4]))
            if moloc[4]=="IDEN":
                file.write(" (");imprime_lk8(format,file,moloc[5],6,decal1=0,decaln=40,retour=0);file.write(")\n")
            else :
                file.write("\n")
                for (en,point) in moloc[5]:
                    file.write(" "*33+en+"   (");imprime_lk8(format,file,point,6,decal1=0,decaln=40,retour=0);
                    file.write(")\n")
        else: raise Exception("erreur")

    file.write( "\nVECTEUR__ " +"\n")
    for moloc in MLVEs:
        file.write( "    "+moloc[0] + " = " + moloc[1] + " " + moloc[2] +"\n")

    file.write( "\nMATRICE__ " +"\n")
    for moloc in MLMAs:
        file.write( "    "+moloc[0] + " = " + moloc[1] + " " + moloc[2] + " " + moloc[3] +"\n")

    file.write( "\nOPTION__ "  +"\n")
    if opts:
       opts.sort()
       for opt in opts :
           file.write( "    %-16s    %-5s" % (opt[0],opt[1]) )
           file.write( " IN__   ")
           if format == 'cata_l' :
              imprime_lk8(format,file,opt[2],8,decaln=37,retour=0)
              file.write( "  OUT__  "   )
           else :
              imprime_lk8(format,file,opt[2],8,decaln=37)
              file.write( " "*29+" OUT__  "   )
           imprime_lk8(format,file,opt[3],8,decaln=37)

    file.write( "\n")


def impr_entete(file,entete,l_elref1,lattrib,l_decl_en,l_decl_opt,format):
     file.write( "\nENTETE__ ")
     file.write( "%s %-16s " %("ELEMENT__",entete[0]))
     file.write( "%s %-8s " %("MAILLE__",entete[1]))

     for elref1 in l_elref1 :
        file.write( "\n   %-s  %-8s" %("ELREFE__",elref1[0]))

        if elref1[1] :
           file.write( "  GAUSS__")
           for attr_val in elref1[1][0] :
              file.write("  %s=%s" %(attr_val[0],attr_val[1]))
           if elref1[1][1] :  # FPG_LISTE__ ...
              for (fpgl,l1) in elref1[1][1] :
                 file.write("  FPG_LISTE__ %s=(" % fpgl )
                 for x in l1 : file.write("%s " % x )
                 file.write(")")

     if lattrib :
         file.write( "\n   ATTRIBUT__",)
         for (x,y) in lattrib :  file.write( "  %s=%s" % (x,y))

     if l_decl_en :
         for decl in l_decl_en :
             str1=''
             for k in decl[1]: str1=str1+"  "+str(k)
             file.write( "\n   %-12s %-6s  =   %s" %("ENS_NOEUD__",decl[0],str1))

     if l_decl_opt :
         for decl in l_decl_opt :
             file.write( "\n   %-10s %-18s      %s" %("OPTION__",decl[0],str(decl[1])))




#------------------------------------------------------------------------------------
# impression au format 'ojb' :
#------------------------------------------------------------------------------------

def imprime_ojb(file,capy):
   ERR.mess('I',"Début de la transformation de l'ENSEMBLE des catalogues en objets jeveux")

#----------------------------------------------------------
# Attention : cette fonction "dégénérise" les TYPE_GENE__ !
#----------------------------------------------------------

   # On transforme le capy pour remplacer les TYPE_GENE__ par des TYPE_ELEM__ :
   degenerise(capy)



   if not capy.tm : ERR.mess('E','il faut un catalogue de TYPE_MAILLE__.')
   if not capy.gd : ERR.mess('E','il faut un catalogue de GRANDEUR__.')
   if not capy.ph : ERR.mess('E','il faut un catalogue de PHENOMENE_MODELISATION__.')
   if len(capy.op)==0 : ERR.mess('E',"il faut des catalogues d'OPTION__.")
   if len(capy.te)==0 : ERR.mess('E',"il faut des catalogues de TYPE_ELEM__.")

   d={} # dictionnaire des ojb

   #=========================================================================================
   # XXUTIL:
   # Bouts de code peuvent servir aux développeurs pour générer des fichiers de "doc" :
   # Ces bouts de code sont placés ici, après le "degenerise" et avant les "del cata"
   if 0 :
      nomfic="/local00/home/lvabhhts/U/liCMP.txt"
      impr_CMP(nomfic,capy) # pour imprimer tous les 6-uplets ( OPTION  TYPELEM  IN/OUT  PARAM   GRANDEUR  CMP )
   if 0 :
      nomfic="/local00/home/lvabhhts/U/param_options.txt"
      # le fichier produit est moins gros que liCMP mais surtout il contient les paramètres RESL
      impr_param_options(nomfic,capy) # pour imprimer tous les 5-uplets ( OPTION  TYPELEM  IN/OUT  PARAM  GRANDEUR)
   if 0 :
      nomfic="/local00/home/lvabhhts/U/PbOptions.txt"
      PbOptions(nomfic,capy) # pour imprimer le nom des parametres inutilises des options
   if 0 :
      nomfic="/local00/home/lvabhhts/U/nomte_nomtm.txt"
      nomte_nomtm(nomfic,capy) # pour imprimer les couples (type_elem, type_maille)
   if 0 :
      nomfic="/local00/home/lvabhhts/U/numte_lnomte.txt"
      numte_lnomte(nomfic,capy) # pour imprimer les lignes (te00ij -> (type_elem1, type_elem2, ...)
   #=========================================================================================


   #  TOUCOMLIBR = objet contenant tous les commentaires libres :
   #-------------------------------------------------------------
   TOUCOMLIBR=ut.cree_co(d,nom='&CATA.CL.COMLIBR',tsca='K80',tsca_pn='K8',contig='CONTIG',acces='NU',longv=1)

   def split_comlibr(TOUCOMLIBR,comlibr):
      # "splite" la chaine comlibr en plusieurs lignes et stocke ces lignes dans TOUCOMLIBR
      indice=len(TOUCOMLIBR.objs)
      if comlibr :
         l=string.split(comlibr,'\n'); nblig=len(l); ind1=indice
         for x in l :
           ind1=ind1+1
           TOUCOMLIBR.cree_oc(nom=str(ind1),long=1)
           TOUCOMLIBR.ecri_co(nom=str(ind1),indice=1,valeur=x)
      else :
         nblig=0
      return (nblig,indice+1)

   def split_localisation(chaineDependance):
      return chaineDependance[1:-1].split("!")

   def elrefe_npg(cata_tm,NOFPG,elrf,f):
     # retourne le nombre de points de la famille f de l'elrefe elrf et le numéro de la famille
     for tm in cata_tm.ltm :
       for elrefe in tm[4] :
          if elrefe[0]==elrf :
             for fam in elrefe[1]:
                if fam[0]==f :
                   ifpg=NOFPG.jenonu(txtpad(8,elrf)+f)
                   return (int(fam[1]),ifpg)
     ERR.mess('E'," famille de Points de Gauss inconnue: "+f+" pour ELREFE: "+elrf)


   #  catalogue des TYPE_MAILLE :
   #-----------------------------------------
   ERR.contexte("Examen du catalogue de TYPE_MAILLE__")
   cata=capy.tm
   NOMTM=ut.cree_pn(d,nom='&CATA.TM.NOMTM',tsca='K8')
   NOELRF=ut.cree_pn(d,nom='&CATA.TM.NOELRF',tsca='K8')
   NOFPG=ut.cree_pn(d,nom='&CATA.TM.NOFPG',tsca='K16')
   NBNO=ut.cree_co(d,nom='&CATA.TM.NBNO',tsca='I',tsca_pn='K8',contig='CONTIG',acces='NO',longv=1)
   TMDIM=ut.cree_co(d,nom='&CATA.TM.TMDIM',tsca='I',tsca_pn='K8',contig='CONTIG',acces='NO',longv=1)
   nbtm=len(cata.ltm)

   nb_elrf=0; nb_fpg=0
   for k in range(nbtm):
       NOMTM.ajout_nom(cata.ltm[k][0])
       NBNO.cree_oc(nom=cata.ltm[k][0],long=1)
       NBNO.ecri_co(nom=cata.ltm[k][0],indice=1,valeur=int(cata.ltm[k][1]))
       TMDIM.cree_oc(nom=cata.ltm[k][0],long=1)
       TMDIM.ecri_co(nom=cata.ltm[k][0],indice=1,valeur=int(cata.ltm[k][2]))
       for elrf in cata.ltm[k][4] :
          nom=elrf[0]
          assert not NOELRF.dico.has_key(nom)
          nb_elrf=nb_elrf+1
          NOELRF.ajout_nom(nom)
          nom=txtpad(8,nom)
          for fam in elrf[1]:
             nb_fpg=nb_fpg+1
             NOFPG.ajout_nom(nom+fam[0])

   TMELRF=ut.cree_os(d,nom='&CATA.TM.TMELRF',tsca='I',long=nb_elrf)
   TMFPG=ut.cree_os(d,nom='&CATA.TM.TMFPG',tsca='I',long=nb_fpg)
   ifpg=0;
   for k in range(nbtm):
       nutyma=NOMTM.jenonu(cata.ltm[k][0])
       for elrf in cata.ltm[k][4] :
          nom=elrf[0];
          ielrf=NOELRF.jenonu(nom)
          TMELRF.ecri_os(indice=ielrf,valeur=nutyma)
          for fam in elrf[1]:
             ifpg=ifpg+1
             TMFPG.ecri_os(indice=ifpg,valeur=int(fam[1]))

   del cata


   #  catalogue des GRANDEUR :
   #-----------------------------------------
   ERR.contexte("Examen du catalogue de GRANDEUR__")
   cata=capy.gd
   nbgd=len(cata.l_gdsimp)+len(cata.l_gdelem)
   NOMGD=ut.cree_pn(d,nom='&CATA.GD.NOMGD',tsca='K8')
   TYPEGD=ut.cree_os(d,nom='&CATA.GD.TYPEGD',tsca='K8',long=nbgd)
   NOMCMP=ut.cree_co(d,nom='&CATA.GD.NOMCMP',tsca='K8',tsca_pn='K8',contig='CONTIG',acces='NO',longv=0)
   DESCRIGD=ut.cree_co(d,nom='&CATA.GD.DESCRIGD',tsca='I',tsca_pn='K8',contig='CONTIG',acces='NU',longv=7)

   # on réalise les grandeurs "union" :
   x={}
   for gd in cata.l_gdsimp :
       if not gd.union :
           x[gd.nom]=gd

   for gd in cata.l_gdsimp :
       if  gd.union :
           gd.lcmp=[]
           gd.tscal=x[gd.lgd[0]].tscal
           for nogd in gd.lgd :
              gd.lcmp.extend(x[nogd].lcmp)
              if gd.tscal != x[nogd].tscal :
                 ERR.mess('E'," type scalaire différent entre: "+gd.nom+" et: "+nogd)
           ERR.veri_pas_doublon('E',gd.lcmp)
   del x

   # on "imprime" toutes les grandeurs simples :
   k=0
   for gd in cata.l_gdsimp :
       k=k+1
       ncmp=len(gd.lcmp)
       nogd=gd.nom
       NOMGD.ajout_nom(nogd)
       TYPEGD.ecri_os(indice=k,valeur=gd.tscal)
       NOMCMP.cree_oc(nom=nogd,long=ncmp)
       DESCRIGD.cree_oc(nom=nogd,long=7)

       (nblcom,indcom)=split_comlibr(TOUCOMLIBR,gd.comlibr)
       DESCRIGD.ecri_co(nom=nogd,indice=6,valeur=nblcom)
       DESCRIGD.ecri_co(nom=nogd,indice=7,valeur=indcom)

       for icmp in range(ncmp):
          NOMCMP.ecri_co(nom=nogd,indice=icmp+1,valeur=gd.lcmp[icmp])

       DESCRIGD.ecri_co(nom=nogd,indice=1,valeur=1)
       DESCRIGD.ecri_co(nom=nogd,indice=3,valeur=(len(gd.lcmp)-1)/30 +1)


   for gd in cata.l_gdelem :
       k=k+1
       nogd=gd.nom
       NOMGD.ajout_nom(nogd)
       NOMCMP.cree_oc(nom=nogd,long=0)
       DESCRIGD.cree_oc(nom=nogd,long=7)

       if len(gd.gdelem)==3 :
           ERR.veri_appartient_liste('F',gd.gdelem[0],NOMGD.valeurs)
           ERR.veri_appartient_liste('F',gd.gdelem[1],NOMGD.valeurs)
           igd1=NOMGD.valeurs.index(gd.gdelem[0])
           igd2=NOMGD.valeurs.index(gd.gdelem[1])

           if gd.gdelem[2]=='MS' :
              DESCRIGD.ecri_co(nom=nogd,indice=1,valeur=4)
           elif gd.gdelem[2]=='MR' :
              DESCRIGD.ecri_co(nom=nogd,indice=1,valeur=5)

           DESCRIGD.ecri_co(nom=nogd,indice=4,valeur=igd1+1)
           DESCRIGD.ecri_co(nom=nogd,indice=5,valeur=igd2+1)
           tsca1=TYPEGD.valeurs[igd1]
           tsca2=TYPEGD.valeurs[igd2]
           if tsca1 != tsca2 : raise Exception("Erreur types incompatibles.")
           tscal=tsca1

       if len(gd.gdelem)==1 :
           ERR.veri_appartient_liste('F',gd.gdelem[0],NOMGD.valeurs)
           igd1=NOMGD.valeurs.index(gd.gdelem[0])
           DESCRIGD.ecri_co(nom=nogd,indice=1,valeur=3)
           DESCRIGD.ecri_co(nom=nogd,indice=4,valeur=igd1+1)
           tscal=TYPEGD.valeurs[igd1]

       TYPEGD.ecri_os(indice=k,valeur=tscal)

   del cata

   #  catalogues des OPTION :
   #-----------------------------------------
   nbop=len(capy.op)
   NOMOP=ut.cree_pn(d,nom='&CATA.OP.NOMOPT',tsca='K16')
   DESCOPT=ut.cree_co(d,nom='&CATA.OP.DESCOPT',tsca='I',tsca_pn='K16',contig='CONTIG',acces='NU',longv=0)
   OPTPARA=ut.cree_co(d,nom='&CATA.OP.OPTPARA',tsca='K8',tsca_pn='K16',contig='CONTIG',acces='NU',longv=0)
   LOCALIS=ut.cree_co(d,nom='&CATA.OP.LOCALIS',tsca='K24',tsca_pn='K16',contig='CONTIG',acces='NU',longv=0)


   for cata in capy.op:
       nom,lchin,lchou,comlibr=cata.cata_op
       ERR.contexte("Examen du catalogue d'OPTION__: "+nom)
       nbin=len(lchin)
       nbou=len(lchou)

       NOMOP.ajout_nom(nom)
       DESCOPT.cree_oc(nom=nom,long=6+3*(nbin+nbou))
       OPTPARA.cree_oc(nom=nom,long=nbin+2*nbou)
       LOCALIS.cree_oc(nom=nom,long=3*nbin)

       DESCOPT.ecri_co(nom=nom,indice=2,valeur=nbin)
       DESCOPT.ecri_co(nom=nom,indice=3,valeur=nbou)
       (nblcom,indcom)=split_comlibr(TOUCOMLIBR,comlibr)
       DESCOPT.ecri_co(nom=nom,indice=4+nbin+nbou+1,valeur=nblcom)
       DESCOPT.ecri_co(nom=nom,indice=4+nbin+nbou+2,valeur=indcom)

       k=0
       for (para,nogd,localis,comlibr) in lchin :
           k=k+1
           igd=NOMGD.jenonu(nogd)
           DESCOPT.ecri_co(nom=nom,indice=4+k,valeur=igd)
           OPTPARA.ecri_co(nom=nom,indice=k,valeur=para)
           if localis != None:
              tabDep = split_localisation(localis)
              LOCALIS.ecri_co(nom=nom,indice=3*k-2,valeur=tabDep[0])
              LOCALIS.ecri_co(nom=nom,indice=3*k-1,valeur=tabDep[1])
              if len(tabDep) == 3:
                 LOCALIS.ecri_co(nom=nom,indice=3*k,valeur=tabDep[2])
              else:
                 LOCALIS.ecri_co(nom=nom,indice=3*k,valeur="NSP")
           else:
              LOCALIS.ecri_co(nom=nom,indice=3*k-2,valeur="VIDE")
              LOCALIS.ecri_co(nom=nom,indice=3*k-1,valeur="VIDE")
              LOCALIS.ecri_co(nom=nom,indice=3*k,valeur="VIDE")
           (nblcom,indcom)=split_comlibr(TOUCOMLIBR,comlibr)
           DESCOPT.ecri_co(nom=nom,indice=6+nbin+nbou+2*(k-1)+1,valeur=nblcom)
           DESCOPT.ecri_co(nom=nom,indice=6+nbin+nbou+2*(k-1)+2,valeur=indcom)

       k=0
       for (para,nogd,typout,comlibr) in lchou :
           k=k+1
           igd=NOMGD.jenonu(nogd)
           DESCOPT.ecri_co(nom=nom,indice=4+nbin+k,valeur=igd)
           OPTPARA.ecri_co(nom=nom,indice=nbin+k,valeur=para)
           OPTPARA.ecri_co(nom=nom,indice=nbin+nbou+k,valeur=typout)
           (nblcom,indcom)=split_comlibr(TOUCOMLIBR,comlibr)
           DESCOPT.ecri_co(nom=nom,indice=6+3*nbin+nbou+2*(k-1)+1,valeur=nblcom)
           DESCOPT.ecri_co(nom=nom,indice=6+3*nbin+nbou+2*(k-1)+2,valeur=indcom)

       del cata



   #  catalogues des TYPE_ELEM__ :
   #-------------------------------------------


   # fonction de calcul d'une suite d'entiers codés correspondant à une liste de CMPS:
   def entiers_codes(note,lcmp,lcmp_gd):
      nbec=(len(lcmp_gd)-1)/30 + 1
      liec=[0]*nbec
      rangav=-1
      for icmp in range(len(lcmp)) :
          ERR.veri_appartient_liste('F',lcmp[icmp],lcmp_gd)
          rangcmp= lcmp_gd.index(lcmp[icmp])
          if rangcmp < rangav : ERR.mess('E'," CMPS dans un ordre incorrect. "+repr(lcmp)+" type_element: "+note)

          rangav=rangcmp
          iec= rangcmp / 30
          puiss= (rangcmp % 30) + 1
          liec[iec]= liec[iec] | 2**puiss
      return liec

   # retourne le nombre total d'ELREFE référencés dans les type_elem :
   # mais cela ne veut pas dire qu'ils sont tous différents !
   def nb_elrefe(capy):
      nb=0
      for cata in capy.te:
          entete=cata.cata_te[0]
          nb=nb+len(entete[2])
      return nb

   # retourne le nombre total de "familles locales de PG" référencées dans les type_elem :
   def nb_loc_fpg(capy):
      nb=0
      for cata in capy.te:
          entete=cata.cata_te[0]
          for elref1 in entete[2] :
            if elref1[1] :
               if elref1[1][0] :
                  nb=nb+len(elref1[1][0])
               if elref1[1][1] :
                  nb=nb+len(elref1[1][1])
      return nb



   nbte=len(capy.te)
   nblocfpg=nb_loc_fpg(capy)

   nbopte=0
   for cata in capy.te:
       entete,modlocs,opts=cata.cata_te
       if opts: nbopte=nbopte+len(opts)

   NOMTE=ut.cree_pn(d,nom='&CATA.TE.NOMTE',tsca='K16')
   TYPEMA=ut.cree_os(d,nom='&CATA.TE.TYPEMA',tsca='K8',long=nbte)
   NOMMOLOC=ut.cree_pn(d,nom='&CATA.TE.NOMMOLOC',tsca='K24')
   MODELOC=ut.cree_co(d,nom='&CATA.TE.MODELOC',tsca='I',tsca_pn='K24',contig='CONTIG',acces='NU',longv=0)
   NBELREFE=ut.cree_os(d,nom='&CATA.TE.NBELREFE',tsca='I',long=2*nbte)
   NOELREFE=ut.cree_os(d,nom='&CATA.TE.NOELREFE',tsca='K8',long=nb_elrefe(capy))
   PNLOCFPG=ut.cree_os(d,nom='&CATA.TE.PNLOCFPG',tsca='K32',long=nblocfpg)
   NOLOCFPG=ut.cree_os(d,nom='&CATA.TE.NOLOCFPG',tsca='I',long=nblocfpg)
   OPTT2=ut.cree_os(d,nom='&CATA.TE.OPTT2',tsca='I',long=2*nbopte)


   OPTMOD=ut.cree_co(d,nom='&CATA.TE.OPTMOD',tsca='I',tsca_pn='K8',contig='CONTIG',acces='NU',longv=0)
   OPTNOM=ut.cree_co(d,nom='&CATA.TE.OPTNOM',tsca='K8',tsca_pn='K8',contig='CONTIG',acces='NU',longv=0)
   CTE_ATTR=ut.cree_co(d,nom='&CATA.TE.CTE_ATTR',tsca='K16',tsca_pn='K16',contig='CONTIG',acces='NU',longv=0)



   # objets FPG_LISTE et NOFPG_LISTE :
   # --------------------------------------
   ERR.contexte("fabrication de l'objet .FPG_LISTE")
   lifpgl=get_lifpgl(capy)   # lifpgl={NOMTE(1:16)//nofpgl(1:8):[nofpg1,nofpg2,...,ELREFE]}
   FPG_LISTE=ut.cree_co(d,nom='&CATA.TE.FPG_LISTE',tsca='K8',tsca_pn='K24',contig='CONTIG',acces='NU',longv=0)
   NOFPG_LISTE=ut.cree_pn(d,nom='&CATA.TE.NOFPG_LISTE',tsca='K24')
   lnofpgl= lifpgl.keys(); lnofpgl.sort()
   for nofpgl in lnofpgl:
      NOFPG_LISTE.ajout_nom(nofpgl)
      l1=lifpgl[nofpgl] ; n1=len(l1); print l1
      FPG_LISTE.cree_oc(nom=nofpgl,long=n1)
      for kk in range(n1):
         FPG_LISTE.ecri_co(nom=nofpgl,indice=kk+1,valeur=l1[kk])


   k=0 ; ioptte=0; ielrefe=0; iflpg=0;
   for cata in capy.te:
       k=k+1
       entete,modlocs,opts=cata.cata_te
       l_elref1=entete[2];l_decl_en=entete[4]
       note=entete[0]
       print "<I> On va traiter le TYPE_ELEM: "+note
       ERR.contexte("Examen du catalogue de TYPE_ELEM__: "+note)
       ERR.contexte("  rubrique: ENTETE__","AJOUT")

       NOMTE.ajout_nom(note)
       nute=NOMTE.jenonu(nom=note)
       if nute != k :  ERR.mess('F',"bizarre !")
       note2=txtpad(16,note)

       notm=entete[1]
       nutm=NOMTM.jenonu(nom=notm)
       if nutm == 0 : raise Exception('Erreur: nom de type_maille inconnu: %s' % notm)
       nno=NBNO.lit_co(nom=notm,indice=1)


       TYPEMA.ecri_os(indice=nute,valeur=entete[1])

       # objets NOELREFE et NBELREFE :
       # ---------------------------------
       kelrefe=0
       for elref1 in entete[2] :
           kelrefe=kelrefe+1;ielrefe=ielrefe+1
           NOELREFE.ecri_os(indice=ielrefe,valeur=elref1[0])
       NBELREFE.ecri_os(indice=2*(nute-1)+1,valeur=kelrefe)
       NBELREFE.ecri_os(indice=2*(nute-1)+2,valeur=ielrefe-kelrefe+1)

       # objets PNLOCFPG et NOLOCFPG :
       # ---------------------------------
       num_elref1=0
       for elref1 in entete[2] :
         num_elref1=num_elref1+1
         if not elref1[1] :  continue

         if elref1[1][0] :   # famille de PG ordinaire
            for attr_val in elref1[1][0] :
               iflpg=iflpg+1
               noflpg=note2+txtpad(8,elref1[0])+txtpad(8,attr_val[0])
               ifpg=NOFPG.jenonu(txtpad(8,elref1[0])+attr_val[1])
               PNLOCFPG.ecri_os(indice=iflpg,valeur=noflpg)
               NOLOCFPG.ecri_os(indice=iflpg,valeur=ifpg)

         if elref1[1][1] :   # famille de PG "liste"
            if num_elref1 != 1 :
               ERR.mess('E',"Utilisation d'une famille de PG 'liste' pour un ELREFE__ non principal: "+elref1[0])
            for fpgl in elref1[1][1] :
               iflpg=iflpg+1
               noflpg=note2+txtpad(8,elref1[0])+txtpad(8,fpgl[0])
               ifpg=0 # pour les familles de PG "liste" : NOLOCFPG(iflpg)=0
               PNLOCFPG.ecri_os(indice=iflpg,valeur=noflpg)
               NOLOCFPG.ecri_os(indice=iflpg,valeur=ifpg)


       # objet CTE_ATTR:
       # ---------------------------------
       liattr=get_liattr(capy,cata)
       nbattr=len(liattr)
       CTE_ATTR.cree_oc(nom=note,long=nbattr)
       for iattr in range(nbattr):
          CTE_ATTR.ecri_co(nom=note,indice=iattr+1,valeur=liattr[iattr])


       # modes locaux :
       # ---------------
       MLOCs,MLVEs,MLMAs=modlocs
       ERR.contexte("Examen du catalogue de TYPE_ELEM__: "+note)
       ERR.contexte("  rubrique: définition des modes locaux ","AJOUT")


       for moloc in MLOCs:
           nomolo=moloc[0];nogd=moloc[1];typept=moloc[2];diff=moloc[4]
           nomolo2=note2+nomolo
           NOMMOLOC.ajout_nom(nomolo2)

           igd=NOMGD.jenonu(nogd)
           nec=DESCRIGD.lit_co(nom=nogd,indice=3)
           lcmp_gd=NOMCMP.objs[igd-1].valeurs

           # calcul de nbpt, nbpt2, nbpt3 (et ifpg pour les familles ELGA):
           if typept == "ELEM__"  :  nbpt=1
           if typept == "ELNO__"  :  nbpt=nno
           if typept == "ELGA__"  :
              nbpt=-999
              nofpg1=moloc[3]
              # on cherche les caractéristiques de la famille nofpg1 : nbpt,ifpg

              # on cherche d'abord dans les familles "simples" définies dans capy.tm :
              for decl in l_elref1[0][1][0] :
                 if decl[0]== nofpg1 : (nbpt,ifpg)=elrefe_npg(capy.tm,NOFPG,entete[2][0][0],decl[1])

              # si on n'a pas trouvé, on cherche dans les familles "liste" :
              if nbpt == -999 :
                 if l_elref1[0][1][1] :
                     for decl_l in l_elref1[0][1][1] :
                        if decl_l[0]== nofpg1 :
                           # on parcourt les familles "simples" de la liste :
                           nbpt_l=0
                           for decl_s in decl_l[1][:-1]:
                                for decl in l_elref1[0][1][0] :
                                   if decl[0]== decl_s :
                                       (nbpt,ifpg)=elrefe_npg(capy.tm,NOFPG,entete[2][0][0],decl[1])
                                       nbpt_l=nbpt_l+nbpt
                           nbpt=nbpt_l
                           ifpg= -(lnofpgl.index(note2+nofpg1)+1)

           if nbpt == -999 :
               ERR.mess('E',"Utilisation d'un nombre de points de Gauss indéfini pour le mode_local: "+nomolo)
               assert 0

           if diff == "IDEN" :
              nbpt2=nbpt
              nbpt3=1
           else:
              nbpt2=nbpt+10000
              nbpt3=nbpt

           if typept == "ELGA__"  :
              MODELOC.cree_oc(nom=nomolo2,long=4+nec*nbpt3+1)
           else:
              MODELOC.cree_oc(nom=nomolo2,long=4+nec*nbpt3)

           if typept == "ELEM__"  :  MODELOC.ecri_co(nom=nomolo2,indice=1,valeur=1)
           if typept == "ELNO__"  :  MODELOC.ecri_co(nom=nomolo2,indice=1,valeur=2)
           if typept == "ELGA__"  :  MODELOC.ecri_co(nom=nomolo2,indice=1,valeur=3)
           MODELOC.ecri_co(nom=nomolo2,indice=2,valeur=igd)
           MODELOC.ecri_co(nom=nomolo2,indice=4,valeur=nbpt2)
           if typept == "ELGA__"  :  MODELOC.ecri_co(nom=nomolo2,indice=4+nec*nbpt3+1,valeur=ifpg)

           if diff == "IDEN" :
              point = moloc[5]
              liec=entiers_codes(note,point,lcmp_gd)
              for kk in range(len(liec)):
                 MODELOC.ecri_co(nom=nomolo2,indice=4+kk+1,valeur=liec[kk])
              nbscal=len(point)*nbpt
              MODELOC.ecri_co(nom=nomolo2,indice=3,valeur=nbscal)
           else:
              nbscal=0
              for (en,point) in moloc[5]:
                 liec=entiers_codes(note,point,lcmp_gd)
                 liste=None
                 for (en2,liste2) in l_decl_en:
                    if en2==en : liste=liste2
                 if not liste :
                   pass # la verif. ci-dessous est trop sévère. Voir fiche REX 18068.
                   #ERR.mess('E',"L' ensemble de noeuds "+en+" est non-défini pour l'élément: "+note)
                 else :
                    for ino in liste :
                       for kk in range(len(liec)):
                          MODELOC.ecri_co(nom=nomolo2,indice=4+(ino-1)*nec+kk+1,valeur=liec[kk])
                       nbscal=nbscal+len(point)
              MODELOC.ecri_co(nom=nomolo2,indice=3,valeur=nbscal)


       for moloc in MLVEs:
           nomolo=moloc[0];nogd=moloc[1];molo1=moloc[2]
           nomolo2=note2+nomolo
           igd=NOMGD.jenonu(nogd)
           NOMMOLOC.ajout_nom(nomolo2)
           MODELOC.cree_oc(nom=nomolo2,long=5)
           MODELOC.ecri_co(nom=nomolo2,indice=1,valeur=4)   # VECTEUR
           MODELOC.ecri_co(nom=nomolo2,indice=2,valeur=igd)
           nbscal=MODELOC.lit_co(nom=note2+molo1,indice=3)
           MODELOC.ecri_co(nom=nomolo2,indice=3,valeur=nbscal)
           MODELOC.ecri_co(nom=nomolo2,indice=4,valeur=NOMMOLOC.jenonu(note2+molo1))


       for moloc in MLMAs:
           nomolo=moloc[0];nogd=moloc[1];molo1=moloc[2];molo2=moloc[3]
           nomolo2=note2+nomolo
           igd=NOMGD.jenonu(nogd)
           type_matrice=DESCRIGD.lit_co(nom=nogd,indice=1)
           NOMMOLOC.ajout_nom(nomolo2)
           MODELOC.cree_oc(nom=nomolo2,long=5)
           MODELOC.ecri_co(nom=nomolo2,indice=1,valeur=5)   # MATRICE
           MODELOC.ecri_co(nom=nomolo2,indice=2,valeur=igd)
           nbsca1=MODELOC.lit_co(nom=note2+molo1,indice=3)
           nbsca2=MODELOC.lit_co(nom=note2+molo2,indice=3)
           if molo2 != molo1 and type_matrice != 5 : raise Exception("Erreur")
           if type_matrice == 4 :
               nbscal=nbsca1*(nbsca1+1)/2
           elif type_matrice == 5 :
               nbscal=nbsca1*nbsca2
           else:
               raise Exception("Erreur")
           MODELOC.ecri_co(nom=nomolo2,indice=3,valeur=nbscal)
           MODELOC.ecri_co(nom=nomolo2,indice=4,valeur=NOMMOLOC.jenonu(note2+molo1))
           MODELOC.ecri_co(nom=nomolo2,indice=5,valeur=NOMMOLOC.jenonu(note2+molo2))


       # options :
       # ---------------
       dico_opt_te={}
       if opts:
            for opt in opts:
                noop=opt[0];numte=int(opt[1]);nbin=len(opt[2])/2;nbou=len(opt[3])/2
                ERR.contexte("Examen du catalogue de TYPE_ELEM__: "+note)
                ERR.contexte("  rubrique: OPTION__ : "+noop,"AJOUT")

                if dico_opt_te.has_key(noop) :
                    ERR.mess('E',"L'option: "+noop+" est définie plusieurs fois pour le TYPE_ELEMENT: "+note)
                else :
                    dico_opt_te[noop]=1

                if numte < 0 :
                    ioptte=ioptte+1
                    nuop=NOMOP.jenonu(nom=noop)
                    OPTT2.ecri_os(indice=2*(ioptte-1)+1,valeur=nuop)
                    OPTT2.ecri_os(indice=2*(ioptte-1)+2,valeur=nute)
                    OPTMOD.cree_oc(nom=str(ioptte),long=3+nbin+nbou)
                    OPTNOM.cree_oc(nom=str(ioptte),long=nbin+nbou)
                    OPTMOD.ecri_co(nom=str(ioptte),indice=1,valeur=numte)
                    OPTMOD.ecri_co(nom=str(ioptte),indice=2,valeur=nbin)
                    OPTMOD.ecri_co(nom=str(ioptte),indice=3,valeur=nbou)

                if numte > 0 :
                    ioptte=ioptte+1
                    nuop=NOMOP.jenonu(nom=noop)
                    OPTT2.ecri_os(indice=2*(ioptte-1)+1,valeur=nuop)
                    OPTT2.ecri_os(indice=2*(ioptte-1)+2,valeur=nute)
                    OPTMOD.cree_oc(nom=str(ioptte),long=3+nbin+nbou)
                    OPTNOM.cree_oc(nom=str(ioptte),long=nbin+nbou)
                    OPTMOD.ecri_co(nom=str(ioptte),indice=1,valeur=numte)
                    OPTMOD.ecri_co(nom=str(ioptte),indice=2,valeur=nbin)
                    OPTMOD.ecri_co(nom=str(ioptte),indice=3,valeur=nbou)

                    for kk in range(nbin):
                        mode =opt[2][2*kk]
                        param=opt[2][2*kk+1]
                        OPTNOM.ecri_co(nom=str(ioptte),indice=kk+1,valeur=param)
                        OPTMOD.ecri_co(nom=str(ioptte),indice=3+kk+1,valeur=NOMMOLOC.jenonu(note2+mode))

                    for kk in range(nbou):
                        mode =opt[3][2*kk]
                        param=opt[3][2*kk+1]
                        OPTNOM.ecri_co(nom=str(ioptte),indice=nbin+kk+1,valeur=param)
                        OPTMOD.ecri_co(nom=str(ioptte),indice=3+nbin+kk+1,valeur=NOMMOLOC.jenonu(note2+mode))

       del cata

   #  catalogue des PHENOMENE_MODELISATION :
   #--------------------------------------------------------
   cata=capy.ph
   ERR.contexte("Examen du catalogue de PHENOMENE_MODELISATION__: ")
   PHENOMENE=ut.cree_pn(d,nom='&CATA.PHENOMENE',tsca='K16')

   for (ph,lmod,codph) in cata.l_pheno:
       PHENOMENE.ajout_nom(ph)
       MODELI=ut.cree_co(d,nom='&CATA.'+ph,tsca='I',tsca_pn='K16',contig='CONTIG',acces='NU',longv=(nbtm+2))
       NOMMODELI=ut.cree_pn(d,nom='&CATA.'+txtpad(13,ph)+'.MODL',tsca='K16')
       for (mod,laffe,codmod,(d1,d2),lattrib) in lmod:
           mod=mod[1:len(mod)-1]
           NOMMODELI.ajout_nom(mod)
           MODELI.cree_oc(nom=mod,long=(nbtm+2))
           MODELI.ecri_co(nom=mod,indice=nbtm+1,valeur=int(d1))
           MODELI.ecri_co(nom=mod,indice=nbtm+2,valeur=int(d2))
           for (tyma,tyel) in laffe:
               MODELI.ecri_co(nom=mod,indice=NOMTM.jenonu(nom=tyma),valeur=NOMTE.jenonu(nom=tyel))

   del cata


   #  impression des obj :
   #-----------------------------------------
   likeys=d.keys(); likeys.sort()
   for nomojb in likeys :
      ojb=d[nomojb]
      ojb.impr(file)
   ERR.mess('I',"Fin de la transformation de l'ENSEMBLE des catalogues en objets jeveux")


def verif_type_gene(capy):
#------------------------------------------------------
#  fait quelques vérifications sur les TYPE_GENE__
#-------------------------------------------------------
    ERR.contexte('Vérification des éléments finis génériques (TYPE_GENE__).')
    for cata in capy.tg:
        nogene,l_entete,modlocs,opts=cata.cata_tg

        # 1. On vérifie qu'un catalogue "générique" a bien un nom de la forme GENER_XXXX:
        if nogene[0:6]!="GENER_" :  ERR.mess('E','Le TYPE_GENE__: '+nogene+' doit avoir un nom de la forme : GENER_XXXX')

        # 2. On veut vérifier que les ensembles de noeuds utilisés dans les modes locaux
        # sont définis dans au moins une entete (fiche REX 18068)
        dico_EN={}
        for entete in l_entete :
            l_decl_en=entete[4] ;
            if l_decl_en :
              for (en,liste2) in l_decl_en: dico_EN[en]=1
        liste_EN=dico_EN.keys()

        MLOCs,MLVEs,MLMAs=modlocs
        for moloc in MLOCs:
           diff=moloc[4]
           if diff == "DIFF" :
              for (en,point) in moloc[5]:
                 if en not in liste_EN :
                    ERR.mess('E',"L' ensemble de noeuds "+en+" n'est pas utilisé dans le catalogue : "+nogene)


def degenerise(capy):
#------------------------------------------------
#  remplace les TYPE_GENE__ par des TYPE_ELEM__ :
#------------------------------------------------
    ERR.contexte('Eclatement des éléments finis génériques (TYPE_GENE__) en des TYPE_ELEM__ ordinaires.')
    verif_type_gene(capy)

    nb_te=len(capy.te) ; dicte2={}
    for k in capy.dicte.keys():
        dicte2[k]='a'

    for cata in capy.tg:
        nogene,l_entete,modlocs,opts=cata.cata_tg

        for entete in l_entete :
            l_decl_opt=entete[5]

            entete=entete[0:5]+(None,)
            cata2=copy.deepcopy(cata) ; del cata2.cata_tg; del cata2.cmodif

            if l_decl_opt :
                opts2=copy.deepcopy(opts)
                for decl in l_decl_opt:
                    nomop1,numte=decl

                    for iopt in range(len(opts2)) :
                        if nomop1==opts2[iopt][0] :
                            opt=(nomop1,numte)+ opts2[iopt][2:]
                            opts2[iopt]=opt
            else:
                opts2=opts
            cata2.cata_te=entete,modlocs,opts2


           # on ajoute le TYPE_ELEM__ meme s'il y en a déjà un de meme nom  (TYPE_GENE__ "plus fort" que TYPE_ELEM__):
           #----------------------------------------------------------------------------------------------------
            note=entete[0]
            if capy.dicte.has_key(note) :
                capy.te[capy.dicte[note]]=cata2
                if dicte2.has_key(note):
                   if dicte2[note]=='a':
                       ERR.mess('E','Le TYPE_ELEM__: '+note+' est re-défini en tant que TYPE_GENE__')
                       dicte2[note]='b'
                   elif dicte2[note]=='b':
                       ERR.mess('E','Le TYPE_ELEM__: '+note+' est défini plusieurs fois en tant que TYPE_GENE__')
                else:
                   dicte2[note]='b'
                   ERR.mess('I','Le TYPE_ELEM__: '+note+' est défini en tant que TYPE_GENE__')
            else :
                capy.dicte[note]=nb_te
                capy.te.append(cata2)
                nb_te=nb_te+1
                dicte2[note]='b'

    # on remet les TYPE_ELEM__   dans l'ordre alphabétique:
    # -----------------------------------------------------------------------------
    likeys= capy.dicte.keys(); likeys.sort(); liste2=[]; dico2={};k=0
    for ke in likeys:
       liste2.append(capy.te[capy.dicte[ke]])
       dico2[ke]=k; k=k+1
    capy.te=liste2 ; capy.dicte=dico2


#---------------------------------------------------------------------------
def get_liattr(capy,cata):
#     retourne la liste des attributs d'un type_element :
#     (y compris les attributs définis au niveau des modélisations)
#---------------------------------------------------------------------------
      entete,modlocs,opts=cata.cata_te
      note  = entete[0]
      tyma1 = entete[1]

      # recherche d'informations sur le type de maille : codtma (K3) + dimension topologique
      for tm in capy.tm.ltm :
         if not tm[0]==tyma1 : continue
         dimtma=tm[2]
         codtma=tm[3]

      dicattr={}

      # attributs définis pour toute la modélisation :
      for (ph,lmod,codph) in capy.ph.l_pheno:
          for (mod,laffe,codmod,(d1,d2),lattrib) in lmod:
             # la modélisation inclut-elle le type_element note ?
             trouve=0
             for (tyma,tyel) in laffe:
                if tyel==note : trouve=1

             if trouve :
                # pour les type_element appartenant à plusieurs modélisations,
                # c'est la dernière modélisation qui impose sa loi (dans quel ordre ?).
                # si cette loi est embetante, il faut redéfinir l'attribut au niveau du type_element
                dicattr['ALIAS8']=str(codph)[1:3]+str(codmod)[1:4]+str(codtma)[1:4]
                dicattr['DIM_TOPO_MAILLE']=str(dimtma)
                assert d1 <= d2 , ("dimensions incohérentes :",d1,d2)
                dicattr['DIM_TOPO_MODELI']=str(d1)
                dicattr['DIM_COOR_MODELI']=str(d2)

                if lattrib :
                   for k in range(len(lattrib)) :
                      no_attr =lattrib[k][0]
                      val_attr=lattrib[k][1]
                      dicattr[no_attr]=val_attr


      # surcharge éventuelle des attributs définis pour le type_element:
      lattrib=entete[3]
      if lattrib :
            for k in range(len(lattrib)) :
               no_attr =lattrib[k][0]
               val_attr=lattrib[k][1]
               dicattr[no_attr]=val_attr

      liattr=[]
      for k in dicattr.keys() :
         liattr.append(k)
         liattr.append(dicattr[k])
      return liattr


#-------------------------------------------------------------------------------------------
def get_lifpgl(capy):
#  retourne un dictionnaire contenant toutes les définitions des familles "liste" de PG
#-------------------------------------------------------------------------------------------
   lifpgl={}
   for cata in capy.te:
       entete,modlocs,opts=cata.cata_te
       l_elref1=entete[2]
       note2=txtpad(16,entete[0])

       for elref1 in l_elref1 :
           if  elref1[1] :
               if  elref1[1][1] :
                   for fpgl in elref1[1][1] :
                       nofpgl=fpgl[0]
                       lifpgl[note2+nofpgl]=fpgl[1]
                       lifpgl[note2+nofpgl].append(elref1[0])
   return lifpgl



#----------------------------------------------------------------------------------
def impr_CMP(nomfic,capy):
# pour imprimer tous les 6-uplets ( OPTION  TYPELEM  IN/OUT  PARAM  GRANDEUR  CMP )
#-----------------------------------------------------------------------------------
   file = open(nomfic,"w")

   for cata in capy.te:
       entete,modlocs,opts=cata.cata_te
       note=entete[0]


       # modes locaux :
       # ---------------
       MLOCs,MLVEs,MLMAs=modlocs

       dicmod={}
       for moloc in MLOCs:
           nomolo=moloc[0];nogd=moloc[1];typept=moloc[2];diff=moloc[4]

           licmp=[]
           if diff == "IDEN" :
              licmp.extend(moloc[5])
           else:
              for (en,point) in moloc[5]:
                 licmp.extend(point)
           licmp=list(set(licmp)) ; licmp.sort()
           dicmod[nomolo]=(nogd,licmp)

       if opts:
            for opt in opts:
                noop=opt[0];numte=int(opt[1]);nbin=len(opt[2])/2;nbou=len(opt[3])/2

                if numte > 0 :

                    for kk in range(nbin):
                        mode =opt[2][2*kk]
                        param=opt[2][2*kk+1]
                        if mode in dicmod.keys() :
                           nogd,licmp=dicmod[mode]
                           for cmp in licmp :
                              file.write(noop+" "+note+" IN "+param+" "+nogd+" "+cmp+"\n")

                    for kk in range(nbou):
                        mode =opt[3][2*kk]
                        param=opt[3][2*kk+1]
                        if mode in dicmod.keys() :
                           nogd,licmp=dicmod[mode]
                           for cmp in licmp :
                              file.write(noop+" "+note+" OUT "+param+" "+nogd+" "+cmp+"\n")



#----------------------------------------------------------------------------------
def impr_param_options(nomfic,capy):
# pour imprimer tous les 5-uplets ( OPTION  TYPELEM  IN/OUT  PARAM  GRANDEUR)  (y compris les RESL)
#-----------------------------------------------------------------------------------
   file = open(nomfic,"w")

   for cata in capy.te:
       entete,modlocs,opts=cata.cata_te
       note=entete[0]

       MLOCs,MLVEs,MLMAs=modlocs

       dicmod={}
       for moloc in MLOCs:
           nomolo=moloc[0];nogd=moloc[1]
           dicmod[nomolo]=nogd
       for moloc in MLVEs:
           nomolo=moloc[0];nogd=moloc[1]
           dicmod[nomolo]=nogd
       for moloc in MLMAs:
           nomolo=moloc[0];nogd=moloc[1]
           dicmod[nomolo]=nogd

       if opts:
            for opt in opts:
                noop=opt[0];numte=int(opt[1]);nbin=len(opt[2])/2;nbou=len(opt[3])/2

                if numte > 0 :

                    for kk in range(nbin):
                        mode =opt[2][2*kk]
                        param=opt[2][2*kk+1]
                        if mode in dicmod.keys() :
                           nogd=dicmod[mode]
                           file.write(noop+" "+note+" IN "+param+" "+nogd+"\n")

                    for kk in range(nbou):
                        mode =opt[3][2*kk]
                        param=opt[3][2*kk+1]
                        if mode in dicmod.keys() :
                           nogd=dicmod[mode]
                           file.write(noop+" "+note+" OUT "+param+" "+nogd+"\n")



#----------------------------------------------------------------------------------
def PbOptions(nomfic,capy):
# pour imprimer les noms des options qui ne sont plus realisees
# pour imprimer les noms des parametres inutilises des options
#-----------------------------------------------------------------------------------
   file = open(nomfic,"w")

   utilise={}
   for cata in capy.te:
       entete,modlocs,opts=cata.cata_te
       note=entete[0]

       if opts:
            for opt in opts:
                noop=opt[0];numte=int(opt[1]);nbin=len(opt[2])/2;nbou=len(opt[3])/2
                if not utilise.has_key(noop) : utilise[noop]=[]
                if numte > 0 :
                    for kk in range(nbin):
                        param=opt[2][2*kk+1]
                        utilise[noop].append(param)
                    for kk in range(nbou):
                        param=opt[3][2*kk+1]
                        utilise[noop].append(param)

   declare={}
   for cata in capy.op:
       noop,lchin,lchou,comlibr=cata.cata_op
       declare[noop]=[]
       for (param,nogd,localis,comlibr) in lchin :
          declare[noop].append(param)
       for (param,nogd,localis,comlibr) in lchou :
          declare[noop].append(param)

   # les parametres declares et non utilises sont a supprimer :
   lopt=declare.keys() ; lopt.sort()
   for noop in lopt:
     if not noop in utilise.keys() :
        file.write("A_DETR "+noop+'\n')
        continue
     for param in declare[noop] :
        if not param in utilise[noop] :
           file.write("INUTILISE "+noop+" "+param+'\n')
        else :
           pass
           #file.write("UTILISE "+noop+" "+param+'\n')



#----------------------------------------------------------------------------------
def numte_lnomte(nomfic,capy):
# pour imprimer les noms des type_element qui utilisent une routine te00ij
#-----------------------------------------------------------------------------------
   file = open(nomfic,"w")
   dico={}
   for cata in capy.te:
       entete,modlocs,opts=cata.cata_te
       note=entete[0]
       if opts:
            for opt in opts:
                numte=int(opt[1])
                if numte > 0 and numte != 99 :
                   numte=1000+numte
                   numte='te0'+str(numte)[1:]
                   if not dico.has_key(numte) : dico[numte]=[]
                   dico[numte].append(note)
   l1=dico.keys(); l1.sort()
   for numte in l1 :
      file.write(numte+' ')
      for note in dico[numte] : file.write(note+' ')
      file.write('\n')


#----------------------------------------------------------------------------------
def nomte_nomtm(nomfic,capy):
# pour imprimer les couples (type_element, type_maille)
#-----------------------------------------------------------------------------------
   file = open(nomfic,"w")
   dico={}
   for cata in capy.te:
       entete,modlocs,opts=cata.cata_te
       note=entete[0]
       notm=entete[1]
       file.write(note+' '+notm+'\n')
