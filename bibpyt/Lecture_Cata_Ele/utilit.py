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
import string,sys,cPickle,copy,os

##################################################################################
#  1    utilitaires pour supprimer ce qui est inutile dans les catalogues
#  2    utilitaires pour vérifications et émission de messages
#  3    utilitaires pour création des objets jeveux en Python
##################################################################################


##################################################################################
#  1    utilitaires pour supprimer ce qui est inutile dans les catalogues
##################################################################################

def menage_capy(capy):
#--------------------------------------
#   pour supprimer ce qui est inutile :
#--------------------------------------
    for cata in capy.tg:
        nom,l_entetg,modlocs,opts=cata.cata_tg
        menage_type_elem(modlocs,opts)
    for cata in capy.te:
        entete,modlocs,opts=cata.cata_te
        menage_type_elem(modlocs,opts)

def menage_type_elem(modlocs,opts):
    menage_te_option(opts)
    menage_te_moloc(modlocs,opts)


def menage_te_option(opts):
#------------------------------------------------------------
#   pour supprimer les paramètres d'une option non calculée :
#------------------------------------------------------------
    if opts:
        for i in range(len(opts)) :
            opt=opts.pop(i)
            numte=int(opt[1])
            if numte < 0 :
                opt= (opt[0],opt[1],['XXXXXX', 'XXXXXX'],['XXXXXX', 'XXXXXX'])
            opts.insert(i,opt)


def menage_te_moloc(modlocs,opts):
#-------------------------------------------------
# pour supprimer les modes locaux inutilisés :
#-------------------------------------------------

    utilise={}; utilise_in={}
    if opts:
        for opt in opts :
            # paramètres "in"
            liste=opt[2]; dim=len(liste)/2
            for kk in range(dim):
                mode =liste[2*kk]
                utilise[mode]=1 ; utilise_in[mode]=1
            # paramètres "out"
            liste=opt[3]; dim=len(liste)/2
            for kk in range(dim):
                mode =liste[2*kk]
                utilise[mode]=1


    MLOCs,MLVEs,MLMAs=modlocs

    # on détruit les modes locaux de type vecteur/matrice inutilisés dans les options :
    for moloc in MLVEs:
        if not utilise.has_key(moloc[0]) :  MLVEs.remove(moloc)
    for moloc in MLMAs:
        if not utilise.has_key(moloc[0]) :  MLMAs.remove(moloc)

    # on ajoute les modes nécessaires aux modes vecteur/matrice :
    for moloc in MLVEs:
        utilise[moloc[2]]=1
    for moloc in MLMAs:
        utilise[moloc[2]]=1
        utilise[moloc[3]]=1

    for moloc in MLOCs:
        # on conserve les modes locaux DDL_MECA, DDL_THER, ...
        if (not utilise.has_key(moloc[0])) and (not moloc[0][0:4] == "DDL_") : MLOCs.remove(moloc)



##################################################################################
#  2    utilitaires pour vérifications et émission de messages
##################################################################################

from Execution.strfunc import convert

class ERREUR:
    def __init__(self):
        self.IER=0
        self.contxt=[]

    def mess(self,code,message):
        # pour imprimer un message d'erreur :
        ucode=string.upper(code)
        if  ucode=='I':
            print "\n<"+ucode+"> INFORMATION: ", convert(message)
        elif  ucode=='A':
            print "\n<"+ucode+"> ALARME: ", convert(message)
        elif  ucode=='E':
            print "\n<"+ucode+"> ERREUR: ", convert(message)
            self.IER=self.IER+1
        elif  ucode=='F':
            print "\n<"+ucode+"> ERREUR: ", convert(message)
            self.IER=self.IER+1
        else : raise StandardError

        if ucode =='E' or ucode=='A' or ucode=='F' :
            if len(self.contxt) > 0 :
                print "    CONTEXTE: "
                for c in self.contxt: print "         "+c
        if ucode =='F' : raise StandardError


    def veri_appartient_liste(self,code,element,liste):
        try :
            x=liste.index(element)
        except ValueError:
            self.mess(code,str(element)+" n'appartient pas à la liste: "+str(liste))


    def veri_pas_doublon(self,code,liste):
        x= {}
        for e in liste :
            if x.has_key(e) : self.mess(code,e +" apparait plusieurs fois dans la liste: "+str(liste))
            x[e]=0

    def veri_long_chaine(self,code,chaine,n):
        if len(chaine) > n :
            self.mess(code,chaine +'est une chaine de caractères trop longue (>'+str(n)+').')

    def veri_new_key(self,code,nom,dico):
        if dico.has_key(nom):
            self.mess(code,'le nom: ' + nom + ' est déjà défini.')
            return 1
        else:
            return 0

    def fini(self):
        # pour demander l'arret du code si necessaire :
        if self.IER > 0 :
            print "<F> ERREUR FATALE déclanchée suite aux erreurs <E> précédentes"
            raise StandardError

    def contexte(self,texte,statut="RAZ"):
        # pour définir le contexte d'un message d'erreur:
        if statut=='RAZ' :
            self.contxt=[]
            if len(texte) > 0 : self.contxt.append(texte)
        elif statut=='AJOUT' :
            self.contxt.append(texte)
        else :
            self.mess('F','erreur pgmeur')

# ERR: objet ERREUR partagé par toute l'application :
# ----------------------------------------------------
ERR=ERREUR()


def cmp_tuple_1(a,b):
    if  a[1] < b[1] :
        return -1
    elif  a[1] > b[1] :
        return 1
    else :
        return 0

def cmp_gd(a,b):
    if  a.nom < b.nom :
        return -1
    elif  a.nom > b.nom :
        return 1
    else :
        return 0


def tronque_n(lmax,liste) :
# pour tronquer une liste de chaines de caratères:
    for x in liste :
        if len(x) > lmax :
            ERR.mess('A',"troncature : avant : "+x+" apres: "+x[0:lmax])
        x = x[0:lmax]
        return liste

def tronque_1(lmax,x) :
# pour tronquer une chaine de caratères:
    if len(x) > lmax :
        ERR.mess('A',"troncature : avant : "+x+" apres: "+x[0:lmax])
    x = x[0:lmax]
    return x


def chaine(var,long,cadre='G'):
    # une petite fonction pour formater les chaines de caratères
    if cadre == 'G':
        return str(var)[0:long]+" "*(long-len(str(var)))
    elif cadre == 'D':
        return " "*(long-len(str(var)))+str(var)[0:long]
    else:
        ERR.mess('F', "Erreur")

##################################################################################
#  3    utilitaires pour création des objets jeveux en Python
##################################################################################


def cree_os(dicobj,nom,tsca,long):
    if dicobj.has_key(nom):  ERR.mess('F', "Erreur objet déjà déclaré:"+nom)
    o1= JV_SIMPLE(nom,tsca,long)
    dicobj[nom]=o1
    return o1

def cree_pn(dicobj,nom,tsca):
    if dicobj.has_key(nom):  ERR.mess('F', "Erreur objet déjà déclaré:"+nom)
    o1= JV_PNOM(nom,tsca)
    dicobj[nom]=o1
    return o1

def cree_co(dicobj,nom,tsca,tsca_pn,contig,acces,longv):
    if dicobj.has_key(nom):  ERR.mess('F', "Erreur objet déjà déclaré:"+nom)
    o1= JV_COLLEC(nom,tsca,tsca_pn,contig,acces,longv)
    dicobj[nom]=o1
    return o1


class JV_COLLEC:

    def __init__(self,nom,tsca,tsca_pn,contig,acces,longv):
    # ----------------------------------------------------------------------------------------
    # pour créer une collection jeveux
    # tsca = /'I' /'R' /'C' /'K8' /'K16' ...
    # tsca_pn = /'K8' /'K16' /'K24' ... : longueur des chaines permettant l'acces aux OC
    # contig = /'CONTIG' /'DISPER'
    # acces = /'NU' /'NO'
    # longv : /0 (si longueur variable) /n (si longueur constante)
        self.nom=nom
        self.typojb='collec'
        self.tsca=tsca
        self.tsca_pn=tsca_pn
        self.acces=acces
        self.contig=contig
        self.longv=longv
        if longv < 0 : ERR.mess('F', "Erreur")
        if acces != 'NO' and  acces != 'NU' : ERR.mess('F', "Erreur")
        if contig != 'CONTIG' and  contig != 'DISPER' : ERR.mess('F', "Erreur")
        self.pn=JV_PNOM(nom=nom,tsca=self.tsca_pn)
        self.objs=[]

    def cree_oc(self,nom,long):
        oc1=JV_SIMPLE(nom,self.tsca,long)
        num=self.pn.jenonu(nom,'COOL')
        if num > 0 : ERR.mess('F', "Erreur : nom existant déjà: "+nom+" dans: "+self.nom)
        if self.longv >0 and long != self.longv : ERR.mess('F', "Erreur : longueur incorrecte: "+
                                                  str(long)+" pour: "+self.nom)
        self.objs.append(oc1)

    def ecri_co(self,nom,indice,valeur):
        num=self.pn.jenonu(nom)
        if num < 0 : ERR.mess('F', "Erreur : nom inexistant: "+nom+" dans: "+self.nom)
        oc1=self.objs[num-1]
        oc1.ecri_os(indice,valeur)

    def lit_co(self,nom,indice):
        num=self.pn.jenonu(nom)
        if num < 0 : ERR.mess('F', "Erreur : nom inexistant: "+nom+" dans: "+self.nom)
        oc1=self.objs[num-1]
        return oc1.lit_os(indice)

    def impr(self,file):
        nmaxoc= len(self.objs)
        lont=0
        if self.contig == 'CONTIG' :
            for oc1 in self.objs: lont=lont+len(oc1.valeurs)
        if self.longv == 0 :
            modlon='VARIABLE'
        else:
            modlon='CONSTANT'

        file.write( "|TYPE_JEVEUX=COLLEC"+"\n")
        file.write( "|NOM="+chaine(self.nom,24)+"|TYPE="+chaine(self.tsca,3)+"|NMAXOC="+chaine(nmaxoc,12,'D')+
                    "|NUTIOC="+chaine(nmaxoc,12,'D')+"|ACCES="+chaine(self.acces,2)+"|STOCKAGE="+chaine(self.contig,8)+
                    "|MODELONG="+chaine(modlon,8)+"|LONMAX="+chaine(self.longv,12,'D')+"|LONT="+chaine(lont,12,'D')+"\n")


        for oc1 in self.objs:
            if self.acces=="NO":
                # les collections ayant leur pointeur de nom en interne ont un accès K8
                file.write( "|NOM="+chaine(oc1.nom,8)+"|LONMAX="+chaine(len(oc1.valeurs),12,'D')+"\n")
            else:
                file.write( "|LONMAX="+chaine(len(oc1.valeurs),12,'D')+"\n")

            if self.tsca[0] == "K":
                for val in oc1.valeurs: file.write(str(val)+"\n")
            elif self.tsca[0] == "I":
                for val in oc1.valeurs: file.write(chaine(val,12,'D')+"\n")
            else:
                ERR.mess('F', "Erreur : programmation à ajouter ...")




class JV_SIMPLE:

    def __init__(self,nom,tsca,long):
    # ----------------------------------------------------------------------------------------
    # pour créer un vecteur jeveux
    # tsca = /'I' /'R' /'C' /'K8' /'K16' ...
    # long : longueur du vecteur
        self.nom=nom
        self.typojb='vecteur'
        self.tsca=tsca
        self.long=long
        if long < 0 : ERR.mess('F', "Erreur")
        if tsca[0] == "I" :
            self.valeurs=[0]*long
        elif tsca[0] == "K" :
            self.valeurs=[""]*long
        else:
            ERR.mess('F', "Erreur")

    def ecri_os(self,indice,valeur):
        if indice <1 or indice > len(self.valeurs) : ERR.mess('F', "Erreur")
        if self.tsca[0]=="K" and type(valeur) != type("a"):  ERR.mess('F', "Erreur : on attend une chaine: "+str(valeur))
        if self.tsca[0]=="I" and type(valeur) != type(1):    ERR.mess('F', "Erreur : on attend un entier: "+str(valeur))
        if self.tsca[0]=="R" and type(valeur) != type(1.e0): ERR.mess('F', "Erreur : on attend un réel: "+str(valeur))

        if self.tsca[0]=="K" :
            # on tronque éventuellement la chaine :
            ERR.veri_long_chaine('E',valeur,int(self.tsca[1:3]))
            nom2=valeur[0:int(self.tsca[1:3])]
            self.valeurs[indice-1]=nom2
        else:
            self.valeurs[indice-1]=valeur

    def lit_os(self,indice):
        if indice <1 or indice > len(self.valeurs) : ERR.mess('F', "Erreur")
        return self.valeurs[indice-1]

    def impr(self,file):
        file.write( "|TYPE_JEVEUX=SIMPLE"+"\n")
        file.write( "|NOM="+chaine(self.nom,24)+"|TYPE="+chaine(self.tsca,3)+"|LONMAX="+chaine(self.long,12,'D')+"\n")
        if self.tsca[0] == "K":
            for val in self.valeurs: file.write(str(val)+"\n")
        elif self.tsca[0] == "I":
            for val in self.valeurs: file.write(chaine(val,12,'D')+"\n")
        else:
            ERR.mess('F', "Erreur : programmation à ajouter ...")


class JV_PNOM:

    def __init__(self,nom,tsca):
    # ----------------------------------------------------------------------------------------
    # pour créer un pointeur de noms jeveux
    # tsca = /'I' /'R' /'C' /'K8' /'K16' ...
        self.nom=nom
        self.typojb='pteur_nom'
        if tsca[0]!="K" :  ERR.mess('F', "Erreur : tsca = K* obligatoire.")
        self.tsca=tsca
        self.valeurs=[]
        self.dico={}
        self.nomuti=0

    def jenonu(self,nom,stop='PAS_COOL'):
    # rend le numéro (num de 1 à n) d'un nom dans un pointeur de noms.
    # num est < 0 si le nom a été ajouté au pointeur.
        if type(nom) != type("a"):  ERR.mess('F', "Erreur : on attend nom=chaine.")
        ERR.veri_long_chaine('E',nom,int(self.tsca[1:3]))
        nom2=nom[0:int(self.tsca[1:3])]
        if self.dico.has_key(nom2) :
            return self.dico[nom2]
        else:
            if stop != "COOL" : ERR.mess('F', "Erreur: nom <"+nom+"> inexistant dans: "+self.nom)
            indice=self.nomuti+1
            self.nomuti=indice
            self.dico[nom2]=indice
            self.valeurs.append(nom2)
            return -indice


    def ajout_nom(self,nom):
    # ajoute un nom dans un pointeur de noms.
    # s'arrete en erreur fatale si le nom existe déjà
        if self.dico.has_key(nom) :
            ERR.mess('F', "Erreur: le nom: "+nom+" existe déjà dans: "+self.nom)
        else:
            indice=self.nomuti+1
            self.nomuti=indice
            ERR.veri_long_chaine('E',nom,int(self.tsca[1:3]))
            nom2=nom[0:int(self.tsca[1:3])]
            self.dico[nom2]=indice
            self.valeurs.append(nom2)

    def impr(self,file):
        file.write( "|TYPE_JEVEUX=PT_NOM"+"\n")
        file.write( "|NOM="+chaine(self.nom,24)+"|TYPE="+chaine(self.tsca,3)+"|NOMMAX="+chaine(self.nomuti,12,'D')+"\n")
        nchar=int(self.tsca[1:4])
        for val in self.valeurs:
            file.write(chaine(val,nchar)+"\n")



#####################################################################################
#  fonctions de manipulation de capy : pickled/unpickled + surcharge  + destruction
# ###################################################################################

def write_capy(capy,nomfic):
#==================================
#   sauver un catalogue python (capy) sur un fichier (cPickle)
    fimpr = open(nomfic,"w")
    cPickle.dump(capy,fimpr)
    fimpr.close()


def read_capy(nomfic):
#==================================
#   récupérer un catalogue python (capy) sur un fichier (un-cPickle)
    fimpr = open(nomfic,"r")
    capy=cPickle.load(fimpr)
    fimpr.close()
    return capy


def surch_capy(capy1,capy2):
#==================================
#   ajouter/remplacer  capy2 à capy1
#   Attention : cette fonction modifie capy1 (enrichissement + remplacement)

    if capy2 is None : return 0

    ERR.mess('I',"Début de la surcharge des catalogues")

    if capy2.gd : capy1.gd=capy2.gd
    if capy2.ph : capy1.ph=capy2.ph
    if capy2.tm : capy1.tm=capy2.tm
    if capy2.mp : capy1.mp=capy2.mp

    for cata in capy2.op :
        nom =cata.cata_op[0]
        num2=capy2.dicop[nom]
        if capy1.dicop.has_key(nom):
            num3=capy1.dicop[nom]
            capy1.op[num3]=copy.deepcopy(capy2.op[num2])
        else:
            num3=len(capy1.dicop)
            capy1.dicop[nom]=num3
            capy1.op.append(copy.deepcopy(capy2.op[num2]))

    for cata in capy2.te :
        nom =cata.cata_te[0][0]
        num2=capy2.dicte[nom]
        if capy1.dicte.has_key(nom):
            num3=capy1.dicte[nom]
            capy1.te[num3]=copy.deepcopy(capy2.te[num2])
        else:
            num3=len(capy1.dicte)
            capy1.dicte[nom]=num3
            capy1.te.append(copy.deepcopy(capy2.te[num2]))

    for cata in capy2.tg :
        nom =cata.cata_tg[0]
        num2=capy2.dictg[nom]
        if capy1.dictg.has_key(nom):
            num3=capy1.dictg[nom]
            capy1.tg[num3]=copy.deepcopy(capy2.tg[num2])
        else:
            num3=len(capy1.dictg)
            capy1.dictg[nom]=num3
            capy1.tg.append(copy.deepcopy(capy2.tg[num2]))

    # on met les options ,les type_elem et les type_gene dans l'ordre alphabétique:
    # -----------------------------------------------------------------------------
    likeys= capy1.dicop.keys(); likeys.sort(); liste2=[]; dico2={};k=0
    for ke in likeys:
        liste2.append(capy1.op[capy1.dicop[ke]])
        dico2[ke]=k; k=k+1
    capy1.op=liste2 ; capy1.dicop=dico2

    likeys= capy1.dicte.keys(); likeys.sort(); liste2=[]; dico2={};k=0
    for ke in likeys:
        liste2.append(capy1.te[capy1.dicte[ke]])
        dico2[ke]=k; k=k+1
    capy1.te=liste2 ; capy1.dicte=dico2

    likeys= capy1.dictg.keys(); likeys.sort(); liste2=[]; dico2={};k=0
    for ke in likeys:
        liste2.append(capy1.tg[capy1.dictg[ke]])
        dico2[ke]=k; k=k+1
    capy1.tg=liste2 ; capy1.dictg=dico2

    ERR.mess('I',"Fin de la surcharge des catalogues")
    return 0


def cata_split(nomfic,prefix,nblig):
#=============================================
#   spliter 1 fichier .cata en plusieurs morceaux
    file=open(nomfic,"r")
    t=file.readlines()

    # on note le numéro des lignes correspondant au découpage
    # ----------------------------------------------------------
    num_lig=[0]
    ico=0
    for i in range(len(t)):
        t1=string.split(t[i])
        if len(t1) > 1 :
            if t1[0]=="%&" and t1[1] in ("MODIF", "AJOUT", "LIBRARY"):
                ico=ico+(i-num_lig[len(num_lig)-1])
                if ico > nblig :
                    ico=0
                    num_lig.append(i)
    num_lig.append(len(t))

    # on écrit les différents fichiers :
    #------------------------------------
    liste_fic=[]
    for ific in range(len(num_lig)-1) :
        nomfic=prefix+'_'+str(ific)+'.cata'
        liste_fic.append(nomfic)
        fil2=open(nomfic,"w")
        for k in range(num_lig[ific+1]-num_lig[ific]):
            fil2.write(t[num_lig[ific]+k])
    return liste_fic


def concat_capy(capy1,capy2):
#==================================
#   ajouter  capy2 à capy1
#   Attention : cette fonction modifie capy1 (enrichissement + remplacement)

    if capy2 is None : return 0

    ERR.mess('I',"Début de la concaténation des catalogues")

    if capy2.gd : capy1.gd=capy2.gd
    if capy2.ph : capy1.ph=capy2.ph
    if capy2.tm : capy1.tm=capy2.tm
    if capy2.mp : capy1.mp=capy2.mp

    for cata in capy2.op :
        nom =cata.cata_op[0]
        num2=capy2.dicop[nom]
        if capy1.dicop.has_key(nom):
            num3=capy1.dicop[nom]
            capy1.op[num3]=copy.deepcopy(capy2.op[num2])
        else:
            num3=len(capy1.dicop)
            capy1.dicop[nom]=num3
            capy1.op.append(copy.deepcopy(capy2.op[num2]))

    for cata in capy2.te :
        nom =cata.cata_te[0][0]
        num2=capy2.dicte[nom]
        if capy1.dicte.has_key(nom):
            num3=capy1.dicte[nom]
            capy1.te[num3]=copy.deepcopy(capy2.te[num2])
        else:
            num3=len(capy1.dicte)
            capy1.dicte[nom]=num3
            capy1.te.append(copy.deepcopy(capy2.te[num2]))

    for cata in capy2.tg :
        nom =cata.cata_tg[0]
        num2=capy2.dictg[nom]
        if capy1.dictg.has_key(nom):
            num3=capy1.dictg[nom]
            capy1.tg[num3]=copy.deepcopy(capy2.tg[num2])
        else:
            num3=len(capy1.dictg)
            capy1.dictg[nom]=num3
            capy1.tg.append(copy.deepcopy(capy2.tg[num2]))


    ERR.mess('I',"Fin de la concaténation des catalogues")
    return 0


def detruire_cata(capy,unigest) :
#==================================
    u"""détruire dans un objet 'capy' les catalogues indiqués dans le fichier
    unigest

    :capy: objet capy à modifier
    :unigest: nom du fichier unigest
    """
    if not unigest or not os.path.isfile(os.path.abspath(unigest)):
        return
    # prise en compte des destructions :
    with open(os.path.abspath(unigest)) as fid:
        for line in fid:
            words = line.split()
            if not words or words[0].upper() != "CATSUPPR":
                continue
            nom = words[1].upper()
            type = words[2].upper()
            if type == "OPTIONS" :
                dico = capy.dicop
                catas = capy.op
            elif type == "TYPELEM":
                dico = capy.dicte
                catas = capy.te
                if nom not in dico:
                    dico = capy.dictg
                    catas = capy.tg
            else:
                # other must not be element catalog or compelem
                assert type in ("COMMANDE", "COMMUN", "ENTETE", "COMPELEM"), type
                continue
            if nom in dico:
                num = dico[nom]
                del dico[nom]
                del catas[num]
                for nom2 in dico:
                    numav = dico[nom2]
                    if numav > num:
                        dico[nom2] = numav - 1
                mess = u"Le catalogue: %s de type: %s a été détruit."
                ERR.mess('I', mess % (nom, type))
            else:
                mess = u"Le catalogue: %s de type : %s ne peut pas etre détruit."
                ERR.mess('E', mess % (nom, type))


##################################################################################
#  fonctions d'inspection des catalogues  :
##################################################################################

def ut_qui_use_cmp(capy,nomgd,nomcmp):
#=====================================


# pour écrire quels sont les triplets (modelisation,typelem,option)
# qui utilisent une CMP d'une grandeur :

    lte=[]
    lopt2=[]
    for cata in capy.te:
        l1=[]
        entete,modlocs,opts=cata.cata_te
        MLOCs,MLVEs,MLMAs=modlocs

        for moloc in MLOCs :
            nomgd2=moloc[1]
            if nomgd != nomgd2: continue
            for point in moloc[5]:
                k = point.count(nomcmp)
                if k ==0: continue
                l1.append(moloc[0])
                break

        for moloc in MLVEs:
            for molo2 in l1:
                if moloc[2] == molo2 :
                    l1.append(moloc[0])
                    break

        for moloc in MLMAs:
            for molo2 in l1:
                if (moloc[2] == molo2) or (moloc[3] == molo2):
                    l1.append(moloc[0])
                    break

        lopt=[]
        for opt in opts:
            for k in l1:
                numte=int(opt[1])
                if numte <0 : continue
                k2= opt[2].count(k)
                if k2 > 0:
                    lopt.append(opt[0])
                    break
                k2= opt[3].count(k)
                if k2 > 0:
                    lopt.append(opt[0])
                    break

        if len(lopt) > 0 :
            lopt.sort()
            print "#jp4 ",nomgd,nomcmp,entete[0],lopt
            lte.append(entete[0])
            lopt2.extend(lopt)


# impression de la liste des type_elem utilisant nomgd,nomcmp:
# ------------------------------------------------------------
    lte.sort()
    print "#jp3 ",nomgd,nomcmp,lte


# impression de la liste des options utilisant nomgd,nomcmp:
# ----------------------------------------------------------------
    dicopt={}
    for opt in lopt2:
        dicopt[opt]=1
    a=dicopt.keys()
    a.sort()
    print "#jp2 ",nomgd,nomcmp,a



# impression de la liste des modélisations utilisant nomgd,nomcmp:
# ----------------------------------------------------------------
    lmodeli={}
    for (ph,lmod) in capy.ph.l_pheno:
        for (mod,laffe) in lmod:
            for te in lte:
                for (tyma,tyel) in laffe:
                    if tyel == te :
                        lmodeli[ph,mod]=1
                        break

    print "#jp1 ",nomgd,nomcmp,lmodeli.keys()

    print "#jp6 ",nomgd,nomcmp,len(lmodeli.keys()),len(a)
