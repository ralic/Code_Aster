#@ MODIF imprime Lecture_Cata_Ele  DATE 18/03/2003   AUTEUR VABHHTS J.PELLET 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
import string,copy,os
from  Lecture_Cata_Ele import utilit
ut=utilit
ERR=ut.ERR


#######################################################################################################
# Fonction principale :
#######################################################################################################

def impr_cata(capy,nomfic,format,seq='oui'):
#==========================================
#   imprimer un catalogue python (capy) sur un fichier à un format donné
#       seq='non' permet de demander le "split" en autant de fichiers qu'il y a de catalogues

        if seq=="oui":
            fimpr = open(nomfic,"w")
        elif seq=="non":
            fimpr=nomfic
            try:
                os.mkdir(nomfic)
            except : pass
        else:
            raise "Erreur"

        if format == 'cata' :
            ut.menage_capy(capy)
            imprime_cata(fimpr,capy,seq,format)
        elif format == 'cata_l' :
            ut.menage_capy(capy)
            imprime_cata(fimpr,capy,seq,format)
        elif format == 'ojb' :
            imprime_ojb(fimpr,capy)
        else :
            raise "Erreur_Fatale"
        if seq=="oui": fimpr.close()
        ERR.fini()


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
       cata=capy.tm
       if cata :
           if seq=="non": file = open(file2+"/type_maille__.cata","w")
           file.write( cata.cmodif+"\n")
           imprime_copyright(file)
           file.write("TYPE_MAILLE__\n")
           for k in range(len(cata.ltm)):
               file.write( "\nMAILLE__ %-8s   %-5s   DIM__  %-3s   CODE__  %-5s\n" %
                         (cata.ltm[k][0],str(cata.ltm[k][1]), cata.ltm[k][2],cata.ltm[k][3]) )
               for elrefe in cata.ltm[k][4]:
                    file.write( "   ELREFE__ %-8s  FAMILLE__ %-8s    %-3s\n" %
                              (elrefe[0],elrefe[1],str(elrefe[2])) )
           file.write( "\n")


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
               for (mod,laffe,codmod) in lmod:
                   file.write( "\n       MODELISATION__  "+mod+"       CODE__  "+codmod+"\n")
                   for (tyma,tyel) in laffe:
                       file.write( "              MAILLE__ "+tyma+"  ELEMENT__ "+tyel+"\n")
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
           for (para,nogd,comlibr) in lchin :
              if not comlibr : comlibr=" "
              file.write( "    %-10s %-10s   %s\n" %(para,nogd,comlibr))
           file.write( "   OUT__ " +"\n")
           for (para,nogd,typout,comlibr) in lchou :
              if not comlibr : comlibr=" "
              file.write( "    %-10s %-10s %-6s   %s\n" %(para,nogd,typout,comlibr))
           file.write( "\n")
           if seq=="non":file.close()


   #  impression des catalogues des TYPE_GENE :
   #-------------------------------------------
       for cata in capy.tg:
           nom,l_entetg,modlocs,opts=cata.cata_tg
           if seq=="non": file = open(file2+"/"+string.lower(nom)+".cata","w")
           file.write( cata.cmodif +"\n")
           imprime_copyright(file)
           file.write( nom +"\n")
           file.write( "TYPE_GENE__\n")


           for entete in l_entetg:
               l_decl_npg=entete[3];l_decl_en=entete[4];l_decl_opt=entete[5]

               file.write( "ENTETE__ ")
               file.write( "%s %-16s " %("ELEMENT__",entete[0]))
               file.write( "%s %-8s " %("MAILLE__",entete[1]))

               str1=''
               for k in entete[2]: str1=str1+"  "+str(k)
               file.write( "%-s%s" %("ELREFE__",str1))

               if l_decl_npg :
                   for decl in l_decl_npg :
                       file.write( "%s %s=%s" %(" NB_GAUSS__",decl[0],str(decl[1])))
               file.write( "\n")

               if l_decl_en :
                   for decl in l_decl_en :
                       str1=''
                       for k in decl[1]: str1=str1+"  "+str(k)
                       file.write( "    %-20s %-20s  =   %s\n" %("ENS_NOEUD__",decl[0],str1))

               if l_decl_opt :
                   for decl in l_decl_opt :
                       file.write( "    %-20s %-20s      %s\n" %("OPTION__",decl[0],str(decl[1])))

           #impression des modes locaux et des options
           impr_moloc_opt(file,modlocs,opts,format)
           if seq=="non":file.close()


   #  impression des catalogues des TYPE_ELEM__ :
   #-------------------------------------------
       for cata in capy.te:
           entete,modlocs,opts=cata.cata_te
           if seq=="non": file = open(file2+"/"+string.lower(entete[0])+".cata","w")
           l_decl_npg=entete[3];l_decl_en=entete[4]
           file.write( cata.cmodif +"\n")
           imprime_copyright(file)
           file.write( entete[0]+"\n\n")
           file.write( "TYPE_ELEM__\n")
           file.write( "ENTETE__ ")
           file.write( "%s %-16s " %("ELEMENT__",entete[0]))
           file.write( "%s %-8s " %("MAILLE__",entete[1]))

           str1=''
           for k in entete[2]: str1=str1+"  "+str(k)
           file.write( "%-s%s" %("ELREFE__",str1))

           if l_decl_npg :
               for decl in l_decl_npg :
                   file.write( "%s %s=%s" %(" NB_GAUSS__",decl[0],str(decl[1])))
           file.write( "\n")

           if l_decl_en :
               for decl in l_decl_en :
                   str1=''
                   for k in decl[1]: str1=str1+"  "+str(k)
                   file.write( "    %-20s %-20s  =   %s\n" %("ENS_NOEUD__",decl[0],str1))


           #impression des modes locaux et des options
           impr_moloc_opt(file,modlocs,opts,format)
           if seq=="non":file.close()



def imprime_copyright(file):
#------------------------------------------------------------
#  impression des 17 lignes de copyright EDF sur le fichier file
#------------------------------------------------------------
   file.write('%            CONFIGURATION MANAGEMENT OF EDF VERSION                        '+"\n")
   file.write('% ======================================================================    '+"\n")
   file.write('% COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG    '+"\n")
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
#-------------------------------------------------
#  impression des modes locaux et des options  pour un TYPE_ELEM__ ou 1 TYPE_GENE__
#-------------------------------------------------
    MLOCs,MLVEs,MLMAs=modlocs

    file.write( "\nMODE_LOCAL__ " +"\n")
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
        else : raise "erreur"

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


   #  toucomlibr = objet contenant tous les commentaires libres :
   #-------------------------------------------------------------
   toucomlibr=ut.cree_co(d,nom='&CATA.CL.COMLIBR',tsca='K80',tsca_pn='K8',contig='CONTIG',acces='NU',longv=1)

   def split_comlibr(toucomlibr,comlibr):
      # "splite" la chaine comlibr en plusieurs lignes et stocke ces lignes dans toucomlibr
      indice=len(toucomlibr.objs)
      if comlibr :
         l=string.split(comlibr,'\n'); nblig=len(l); ind1=indice
         for x in l :
           ind1=ind1+1
           toucomlibr.cree_oc(nom=str(ind1),long=1)
           toucomlibr.ecri_co(nom=str(ind1),indice=1,valeur=x)
      else :
         nblig=0
      return (nblig,indice+1)

   def elrefe_npg(cata_tm,nofpg,elrf,f):
     # retourne le nombre de points de la famille f de l'elrefe elrf et le numéro de la famille
     for tm in cata_tm.ltm :
       for elrefe in tm[4] :
          if elrefe[0]==elrf and elrefe[1]==f :
             ifpg=nofpg.jenonu(elrf+(8-len(elrf))*' '+f)
             return (int(elrefe[2]),ifpg)
     ERR.mess('E'," famille de Points de Gauss inconnue: "+f+" pour ELREFE: "+elrf)


   #  catalogue des TYPE_MAILLE :
   #-----------------------------------------
   ERR.contexte("Examen du catalogue de TYPE_MAILLE__")
   cata=capy.tm
   nomtm=ut.cree_pn(d,nom='&CATA.TM.NOMTM',tsca='K8')
   noelrf=ut.cree_pn(d,nom='&CATA.TM.NOELRF',tsca='K8')
   nofpg=ut.cree_pn(d,nom='&CATA.TM.NOFPG',tsca='K16')
   nbno=ut.cree_co(d,nom='&CATA.TM.NBNO',tsca='I',tsca_pn='K8',contig='CONTIG',acces='NO',longv=1)
   nbtm=len(cata.ltm)

   nb_elrf=0; nb_fpg=0
   for k in range(nbtm):
       nomtm.ajout_nom(cata.ltm[k][0])
       nbno.cree_oc(nom=cata.ltm[k][0],long=1)
       nbno.ecri_co(nom=cata.ltm[k][0],indice=1,valeur=int(cata.ltm[k][1]))
       for elrf in cata.ltm[k][4] :
          nom=elrf[0]
          if not noelrf.dico.has_key(nom) :
             nb_elrf=nb_elrf+1
             noelrf.ajout_nom(nom)
          nom=nom+(8-len(nom))*' '
          nofpg.ajout_nom(nom+elrf[1])
          nb_fpg=nb_fpg+1

   tmelrf=ut.cree_os(d,nom='&CATA.TM.TMELRF',tsca='I',long=nb_elrf)
   tmfpg=ut.cree_os(d,nom='&CATA.TM.TMFPG',tsca='I',long=nb_fpg)
   ifpg=0;
   for k in range(nbtm):
       nutyma=nomtm.jenonu(cata.ltm[k][0])
       for elrf in cata.ltm[k][4] :
          nom=elrf[0]; ifpg=ifpg+1
          ielrf=noelrf.jenonu(nom)
          tmelrf.ecri_os(indice=ielrf,valeur=nutyma)
          tmfpg.ecri_os(indice=ifpg,valeur=int(elrf[2]))

   del cata


   #  catalogue des GRANDEUR :
   #-----------------------------------------
   ERR.contexte("Examen du catalogue de GRANDEUR__")
   cata=capy.gd
   nbgd=len(cata.l_gdsimp)+len(cata.l_gdelem)
   nomgd=ut.cree_pn(d,nom='&CATA.GD.NOMGD',tsca='K8')
   typegd=ut.cree_os(d,nom='&CATA.GD.TYPEGD',tsca='K8',long=nbgd)
   nomcmp=ut.cree_co(d,nom='&CATA.GD.NOMCMP',tsca='K8',tsca_pn='K8',contig='CONTIG',acces='NO',longv=0)
   descrigd=ut.cree_co(d,nom='&CATA.GD.DESCRIGD',tsca='I',tsca_pn='K8',contig='CONTIG',acces='NU',longv=7)

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
       nomgd.ajout_nom(nogd)
       typegd.ecri_os(indice=k,valeur=gd.tscal)
       nomcmp.cree_oc(nom=nogd,long=ncmp)
       descrigd.cree_oc(nom=nogd,long=7)

       (nblcom,indcom)=split_comlibr(toucomlibr,gd.comlibr)
       descrigd.ecri_co(nom=nogd,indice=6,valeur=nblcom)
       descrigd.ecri_co(nom=nogd,indice=7,valeur=indcom)

       for icmp in range(ncmp):
          nomcmp.ecri_co(nom=nogd,indice=icmp+1,valeur=gd.lcmp[icmp])

       descrigd.ecri_co(nom=nogd,indice=1,valeur=1)
       descrigd.ecri_co(nom=nogd,indice=3,valeur=(len(gd.lcmp)-1)/30 +1)


   for gd in cata.l_gdelem :
       k=k+1
       nogd=gd.nom
       nomgd.ajout_nom(nogd)
       nomcmp.cree_oc(nom=nogd,long=0)
       descrigd.cree_oc(nom=nogd,long=7)

       if len(gd.gdelem)==3 :
           ERR.veri_appartient_liste('F',gd.gdelem[0],nomgd.valeurs)
           ERR.veri_appartient_liste('F',gd.gdelem[1],nomgd.valeurs)
           igd1=nomgd.valeurs.index(gd.gdelem[0])
           igd2=nomgd.valeurs.index(gd.gdelem[1])

           if gd.gdelem[2]=='MS' :
              descrigd.ecri_co(nom=nogd,indice=1,valeur=4)
           elif gd.gdelem[2]=='MR' :
              descrigd.ecri_co(nom=nogd,indice=1,valeur=5)

           descrigd.ecri_co(nom=nogd,indice=4,valeur=igd1+1)
           descrigd.ecri_co(nom=nogd,indice=5,valeur=igd2+1)
           tsca1=typegd.valeurs[igd1]
           tsca2=typegd.valeurs[igd2]
           if tsca1 != tsca2 : raise "Erreur types incompatibles."
           tscal=tsca1

       if len(gd.gdelem)==1 :
           ERR.veri_appartient_liste('F',gd.gdelem[0],nomgd.valeurs)
           igd1=nomgd.valeurs.index(gd.gdelem[0])
           descrigd.ecri_co(nom=nogd,indice=1,valeur=3)
           descrigd.ecri_co(nom=nogd,indice=4,valeur=igd1+1)
           tscal=typegd.valeurs[igd1]

       typegd.ecri_os(indice=k,valeur=tscal)

   del cata

   #  catalogues des OPTION :
   #-----------------------------------------
   nbop=len(capy.op)
   nomop=ut.cree_pn(d,nom='&CATA.OP.NOMOPT',tsca='K16')
   descopt=ut.cree_co(d,nom='&CATA.OP.DESCOPT',tsca='I',tsca_pn='K16',contig='CONTIG',acces='NU',longv=0)
   optpara=ut.cree_co(d,nom='&CATA.OP.OPTPARA',tsca='K8',tsca_pn='K16',contig='CONTIG',acces='NU',longv=0)


   for cata in capy.op:
       nom,lchin,lchou,comlibr=cata.cata_op
       ERR.contexte("Examen du catalogue d'OPTION__: "+nom)
       nbin=len(lchin)
       nbou=len(lchou)

       nomop.ajout_nom(nom)
       descopt.cree_oc(nom=nom,long=6+3*(nbin+nbou))
       optpara.cree_oc(nom=nom,long=nbin+2*nbou)

       descopt.ecri_co(nom=nom,indice=2,valeur=nbin)
       descopt.ecri_co(nom=nom,indice=3,valeur=nbou)
       (nblcom,indcom)=split_comlibr(toucomlibr,comlibr)
       descopt.ecri_co(nom=nom,indice=4+nbin+nbou+1,valeur=nblcom)
       descopt.ecri_co(nom=nom,indice=4+nbin+nbou+2,valeur=indcom)

       k=0
       for (para,nogd,comlibr) in lchin :
           k=k+1
           igd=nomgd.jenonu(nogd)
           descopt.ecri_co(nom=nom,indice=4+k,valeur=igd)
           optpara.ecri_co(nom=nom,indice=k,valeur=para)
           (nblcom,indcom)=split_comlibr(toucomlibr,comlibr)
           descopt.ecri_co(nom=nom,indice=6+nbin+nbou+2*(k-1)+1,valeur=nblcom)
           descopt.ecri_co(nom=nom,indice=6+nbin+nbou+2*(k-1)+2,valeur=indcom)

       k=0
       for (para,nogd,typout,comlibr) in lchou :
           k=k+1
           igd=nomgd.jenonu(nogd)
           descopt.ecri_co(nom=nom,indice=4+nbin+k,valeur=igd)
           optpara.ecri_co(nom=nom,indice=nbin+k,valeur=para)
           optpara.ecri_co(nom=nom,indice=nbin+nbou+k,valeur=typout)
           (nblcom,indcom)=split_comlibr(toucomlibr,comlibr)
           descopt.ecri_co(nom=nom,indice=6+3*nbin+nbou+2*(k-1)+1,valeur=nblcom)
           descopt.ecri_co(nom=nom,indice=6+3*nbin+nbou+2*(k-1)+2,valeur=indcom)

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
          if rangcmp < rangav : ERR.mess('A'," CMPS dans un ordre incorrect. "+repr(lcmp)+" type_element: "+note)

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



   nbte=len(capy.te)

   nomte=ut.cree_pn(d,nom='&CATA.TE.NOMTE',tsca='K16')
   typema=ut.cree_os(d,nom='&CATA.TE.TYPEMA',tsca='K8',long=nbte)
   nommoloc=ut.cree_pn(d,nom='&CATA.TE.NOMMOLOC',tsca='K24')
   modeloc=ut.cree_co(d,nom='&CATA.TE.MODELOC',tsca='I',tsca_pn='K24',contig='CONTIG',acces='NU',longv=0)
   nbelrefe=ut.cree_os(d,nom='&CATA.TE.NBELREFE',tsca='I',long=2*nbte)
   noelrefe=ut.cree_os(d,nom='&CATA.TE.NOELREFE',tsca='K8',long=nb_elrefe(capy))


   nbopte=0
   for cata in capy.te:
       entete,modlocs,opts=cata.cata_te
       if opts: nbopte=nbopte+len(opts)
   optt2=ut.cree_os(d,nom='&CATA.TE.OPTT2',tsca='I',long=2*nbopte)


   optmod=ut.cree_co(d,nom='&CATA.TE.OPTMOD',tsca='I',tsca_pn='K8',contig='CONTIG',acces='NU',longv=0)
   optnom=ut.cree_co(d,nom='&CATA.TE.OPTNOM',tsca='K8',tsca_pn='K8',contig='CONTIG',acces='NU',longv=0)


   k=0 ; ioptte=0; ielrefe=0;
   for cata in capy.te:
       k=k+1
       entete,modlocs,opts=cata.cata_te
       l_decl_npg=entete[3];l_decl_en=entete[4]
       note=entete[0]
       print "<I> On va traiter le TYPE_ELEM: "+note
       ERR.contexte("Examen du catalogue de TYPE_ELEM__: "+note)
       ERR.contexte("  rubrique: ENTETE__","AJOUT")

       nomte.ajout_nom(note)
       nute=nomte.jenonu(nom=note)
       if nute != k :  ERR.mess('F',"bizarre !")
       note2=note+" "*(16-len(note))

       notm=entete[1]
       nutm=nomtm.jenonu(nom=notm)
       if nutm == 0 : raise 'Erreur: nom de type_maille inconnu: '+ notm
       nno=nbno.lit_co(nom=notm,indice=1)


       typema.ecri_os(indice=nute,valeur=entete[1])

       # objets noelrefe et nbelrefe :
       kelrefe=0
       for elrefe in entete[2] :
         kelrefe=kelrefe+1;ielrefe=ielrefe+1
         noelrefe.ecri_os(indice=ielrefe,valeur=elrefe)
       nbelrefe.ecri_os(indice=2*(nute-1)+1,valeur=kelrefe)
       nbelrefe.ecri_os(indice=2*(nute-1)+2,valeur=ielrefe-kelrefe+1)


       # modes locaux :
       # ---------------
       MLOCs,MLVEs,MLMAs=modlocs
       ERR.contexte("Examen du catalogue de TYPE_ELEM__: "+note)
       ERR.contexte("  rubrique: définition des modes locaux ","AJOUT")


       for moloc in MLOCs:
           nomolo=moloc[0];nogd=moloc[1];typept=moloc[2];diff=moloc[4]
           nomolo2=note2+nomolo
           nommoloc.ajout_nom(nomolo2)

           igd=nomgd.jenonu(nogd)
           nec=descrigd.lit_co(nom=nogd,indice=3)
           lcmp_gd=nomcmp.objs[igd-1].valeurs

           # calcul de nbpt, nbpt2 et nbpt3 :
           if typept == "ELEM__"  :  nbpt=1
           if typept == "ELNO__"  :  nbpt=nno
           if typept == "ELGA__"  :
              nbpt=-999
              for decl in l_decl_npg :
                 if decl[0]== moloc[3] : (nbpt,ifpg)=elrefe_npg(capy.tm,nofpg,entete[2][0],decl[1])
           if nbpt == -999 : ERR.mess('E',"Utilisation d'un nombre de points de Gauss indéfini.")

           if diff == "IDEN" :
              nbpt2=nbpt
              nbpt3=1
           else:
              nbpt2=nbpt+10000
              nbpt3=nbpt

           if typept == "ELGA__"  :
              modeloc.cree_oc(nom=nomolo2,long=4+nec*nbpt3+1)
           else:
              modeloc.cree_oc(nom=nomolo2,long=4+nec*nbpt3)

           if typept == "ELEM__"  :  modeloc.ecri_co(nom=nomolo2,indice=1,valeur=1)
           if typept == "ELNO__"  :  modeloc.ecri_co(nom=nomolo2,indice=1,valeur=2)
           if typept == "ELGA__"  :  modeloc.ecri_co(nom=nomolo2,indice=1,valeur=3)
           modeloc.ecri_co(nom=nomolo2,indice=2,valeur=igd)
           modeloc.ecri_co(nom=nomolo2,indice=4,valeur=nbpt2)
           if typept == "ELGA__"  :  modeloc.ecri_co(nom=nomolo2,indice=4+nec*nbpt3+1,valeur=ifpg)

           if diff == "IDEN" :
              point = moloc[5]
              liec=entiers_codes(note,point,lcmp_gd)
              for kk in range(len(liec)):
                 modeloc.ecri_co(nom=nomolo2,indice=4+kk+1,valeur=liec[kk])
              nbscal=len(point)*nbpt
              modeloc.ecri_co(nom=nomolo2,indice=3,valeur=nbscal)
           else:
              nbscal=0
              for (en,point) in moloc[5]:
                 liec=entiers_codes(note,point,lcmp_gd)
                 liste=None
                 for (en2,liste2) in l_decl_en:
                    if en2==en : liste=liste2
                 if liste :
                    for ino in liste :
                       for kk in range(len(liec)):
                          modeloc.ecri_co(nom=nomolo2,indice=4+(ino-1)*nec+kk+1,valeur=liec[kk])
                       nbscal=nbscal+len(point)
              modeloc.ecri_co(nom=nomolo2,indice=3,valeur=nbscal)


       for moloc in MLVEs:
           nomolo=moloc[0];nogd=moloc[1];molo1=moloc[2]
           nomolo2=note2+nomolo
           igd=nomgd.jenonu(nogd)
           nommoloc.ajout_nom(nomolo2)
           modeloc.cree_oc(nom=nomolo2,long=5)
           modeloc.ecri_co(nom=nomolo2,indice=1,valeur=4)   # VECTEUR
           modeloc.ecri_co(nom=nomolo2,indice=2,valeur=igd)
           nbscal=modeloc.lit_co(nom=note2+molo1,indice=3)
           modeloc.ecri_co(nom=nomolo2,indice=3,valeur=nbscal)
           modeloc.ecri_co(nom=nomolo2,indice=4,valeur=nommoloc.jenonu(note2+molo1))


       for moloc in MLMAs:
           nomolo=moloc[0];nogd=moloc[1];molo1=moloc[2];molo2=moloc[3]
           nomolo2=note2+nomolo
           igd=nomgd.jenonu(nogd)
           type_matrice=descrigd.lit_co(nom=nogd,indice=1)
           nommoloc.ajout_nom(nomolo2)
           modeloc.cree_oc(nom=nomolo2,long=5)
           modeloc.ecri_co(nom=nomolo2,indice=1,valeur=5)   # MATRICE
           modeloc.ecri_co(nom=nomolo2,indice=2,valeur=igd)
           nbsca1=modeloc.lit_co(nom=note2+molo1,indice=3)
           nbsca2=modeloc.lit_co(nom=note2+molo2,indice=3)
           if molo2 != molo1 and type_matrice != 5 : raise "Erreur"
           if type_matrice == 4 :
               nbscal=nbsca1*(nbsca1+1)/2
           elif type_matrice == 5 :
               nbscal=nbsca1*nbsca2
           else:
               raise "Erreur"
           modeloc.ecri_co(nom=nomolo2,indice=3,valeur=nbscal)
           modeloc.ecri_co(nom=nomolo2,indice=4,valeur=nommoloc.jenonu(note2+molo1))
           modeloc.ecri_co(nom=nomolo2,indice=5,valeur=nommoloc.jenonu(note2+molo2))


       # options :
       # ---------------
       if opts:
            for opt in opts:
                noop=opt[0];numte=int(opt[1]);nbin=len(opt[2])/2;nbou=len(opt[3])/2
                ERR.contexte("Examen du catalogue de TYPE_ELEM__: "+note)
                ERR.contexte("  rubrique: OPTION__ : "+noop,"AJOUT")
                if numte > 0 :
                    ioptte=ioptte+1
                    nuop=nomop.jenonu(nom=noop)
                    optt2.ecri_os(indice=2*(ioptte-1)+1,valeur=nuop)
                    optt2.ecri_os(indice=2*(ioptte-1)+2,valeur=nute)
                    optmod.cree_oc(nom=str(ioptte),long=3+nbin+nbou)
                    optnom.cree_oc(nom=str(ioptte),long=nbin+nbou)
                    optmod.ecri_co(nom=str(ioptte),indice=1,valeur=numte)
                    optmod.ecri_co(nom=str(ioptte),indice=2,valeur=nbin)
                    optmod.ecri_co(nom=str(ioptte),indice=3,valeur=nbou)

                    for kk in range(nbin):
                        mode =opt[2][2*kk]
                        param=opt[2][2*kk+1]
                        optnom.ecri_co(nom=str(ioptte),indice=kk+1,valeur=param)
                        optmod.ecri_co(nom=str(ioptte),indice=3+kk+1,valeur=nommoloc.jenonu(note2+mode))

                    for kk in range(nbou):
                        mode =opt[3][2*kk]
                        param=opt[3][2*kk+1]
                        optnom.ecri_co(nom=str(ioptte),indice=nbin+kk+1,valeur=param)
                        optmod.ecri_co(nom=str(ioptte),indice=3+nbin+kk+1,valeur=nommoloc.jenonu(note2+mode))

       del cata

   #  catalogue des PHENOMENES_MODELISATION :
   #--------------------------------------------------------
   cata=capy.ph
   ERR.contexte("Examen du catalogue de PHENOMENE_MODELISATION__: ")
   phenomene=ut.cree_pn(d,nom='&CATA.PHENOMENE',tsca='K16')

   for (ph,lmod,codph) in cata.l_pheno:
       phenomene.ajout_nom(ph)
       modeli=ut.cree_co(d,nom='&CATA.'+ph,tsca='I',tsca_pn='K16',contig='CONTIG',acces='NU',longv=nbtm)
       nommodeli=ut.cree_pn(d,nom='&CATA.'+ph+" "*(19-6-len(ph))+'.MODL',tsca='K16')
       for (mod,laffe,codmod) in lmod:
           mod=mod[1:len(mod)-1]
           nommodeli.ajout_nom(mod)
           modeli.cree_oc(nom=mod,long=nbtm)
           for (tyma,tyel) in laffe:
               modeli.ecri_co(nom=mod,indice=nomtm.jenonu(nom=tyma),valeur=nomte.jenonu(nom=tyel))

   del cata


   #  impression des obj :
   #-----------------------------------------
   for nomojb in d.keys() :
      ojb=d[nomojb]
      ojb.impr(file)
   ERR.mess('I',"Fin de la transformation de l'ENSEMBLE des catalogues en objets jeveux")


def degenerise(capy):
#-------------------------------------------
#  remplace les TYPE_GENE__ par des TYPE_ELEM__ :
#-------------------------------------------
    ERR.contexte('Eclatement des éléments finis génériques (TYPE_GENE__) en des TYPE_ELEM__ ordinaires.')
    nb_te=len(capy.te) ; dicte2={}
    for k in capy.dicte.keys():
        dicte2[k]='a'

    for cata in capy.tg:
        nogene,l_entetg,modlocs,opts=cata.cata_tg

        # on vérifie qu'un catalogue "générique" a bien un nom de la forme GENER_XXXX:
        if nogene[0:6]!="GENER_" :  ERR.mess('E','Le TYPE_GENE__: '+nogene+' doit avoir un nom de la forme : GENER_XXXX')

        for entete in l_entetg:
            l_decl_opt=entete[5]

            entetg=entete[0:5]+(None,)
            cata2=copy.deepcopy(cata) ; del cata2.cata_tg; del cata2.cmodif

            if l_decl_opt :
                opts2=copy.deepcopy(opts)
                for decl in l_decl_opt:
                    nomop,numte=decl


                    for iopt in range(len(opts2)) :
                        if nomop==opts2[iopt][0] :
                            opt=(nomop,numte)+ opts2[iopt][2:]
                            opts2[iopt]=opt
            else:
                opts2=opts
            cata2.cata_te=entetg,modlocs,opts2


           # on ajoute le TYPE_ELEM__ meme s'il y en a déjà un de meme nom  (TYPE_GENE__ "plus fort" que TYPE_ELEM__):
           #----------------------------------------------------------------------------------------------------
            note=entete[0]
            if capy.dicte.has_key(note) :
                capy.te[capy.dicte[note]]=cata2
                if dicte2.has_key(note):
                   if dicte2[note]=='a':
                       ERR.mess('A','Le TYPE_ELEM__: '+note+' est re-défini en tant que TYPE_GENE__')
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

    # on remet les  les TYPE_ELEM__   dans l'ordre alphabétique:
    # -----------------------------------------------------------------------------
    likeys= capy.dicte.keys(); likeys.sort(); liste2=[]; dico2={};k=0
    for ke in likeys:
       liste2.append(capy.te[capy.dicte[ke]])
       dico2[ke]=k; k=k+1
    capy.te=liste2 ; capy.dicte=dico2
