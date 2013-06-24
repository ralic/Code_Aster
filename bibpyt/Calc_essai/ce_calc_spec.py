# coding=utf-8
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

# person_in_charge: charles.bodel at edf.fr

import numpy

from Tkinter import Frame, Menubutton, Checkbutton, Menu, StringVar, IntVar
from Tkinter import Scrollbar, Label, Radiobutton, Button, Entry
from Tkinter import Checkbutton, Listbox, Toplevel, Message
import tkFont

from Accas import _F
import aster
from Macro.calc_spec_ops import FonctionError
from Cata.cata import DETRUIRE, CREA_TABLE, CALC_SPEC, RECU_FONCTION,IMPR_TABLE
from Calc_essai.cata_ce import InterSpectre, CreaTable, Tempo
from Calc_essai.outils_ihm import MultiList, DispFRFDialogue, VisuSpectre
from Utilitai.Utmess import UTMESS, MESSAGE_LOGGER

from SD.sd_fonction import sd_fonction

# MESSAGE_LOGGER = classe permettant de formatter et d'afficher les messages d'erreur
mess = MESSAGE_LOGGER()


########################
#                      #
#  CLASSES GRAPHIQUES  #
#                      #
########################




class InterfaceCalcSpec(Frame):

    """
    Classe qui fabrique l'interface graphique permettant de realiser une identification
    d'efforts turbulents, et de controler les calculs. On y trouve les methodes suivantes :
     - choix_donnees, choix_projection, frame_meth, _choix_methode, visu_resu : definition
       frame d'interface graphique,
     - get_list, plot_curve : resultats a visualiser, affichage des courbes,
     - calculate_force : dirige les calculs (effectues dans la classe CalculInverse)
     - crea_champ, proj_champ : utilitaires utilisant les op aster du meme nom
    """


    def __init__(self, root, aster_objects, mess, param_visu):
        Frame.__init__(self,root,borderwidth=4)

        self.objects = aster_objects

        self.font1 = tkFont.Font(family="Helvetica", size=16)

        self.mess = mess
        self.param_visu=param_visu
        self.Spec=None,
        self.Coh=None,
        self.FRF=None,
        self.Cur_Tab=None,
        self.list_temp = aster_objects.list_tempo
        self.InterfaceCalcSpec()


    def InterfaceCalcSpec(self) :

        # Cadre de titre

        ft=Frame(self, borderwidth=4)

        lab = Label(ft,text="Traitement du signal",pady=5, font=self.font1 )
        lab.grid(row=0, column=0, columnspan = 2)
        ft.grid(row=0,sticky='ew')

        # definition des listes de temporels

        fl=Frame(self,relief='sunken', borderwidth=1)
        r=0 # row index

        Label(fl,text="Tables disponibles :",pady=5).grid(row=r,column=0)
        Label(fl,text="Points de mesures :",pady=5).grid(row=r,column=2)
        Label(fl,text=u"Points de référence :",pady=5).grid(row=r,column=4)

        r=r+1

        # appeler refresh quand on change de selection dans list_tab #

        self.list_tab = Listbox(fl,selectmode='browse',background='white')
        for item in  self.list_temp.keys() : self.list_tab.insert('end',item)
        self.list_tab.grid(row=r,column=0)


        self.list_mes = Listbox(fl,selectmode='extended',exportselection=False,background='white')
        self.list_mes.grid(row=r,column=2)

        self.list_ref = Listbox(fl,selectmode='extended',exportselection=False,background='white')
        self.list_ref.grid(row=r,column=4)

        #-- pour s'assurer qu'on gerera bien la table qu'on vient d'exporter, et pas une autre.

        self.list_tab_cur=-1
        self.sel_mes = Button(fl, text="=>", command=lambda:
                                  self.refresh(self.list_temp,self.list_tab,
                                               self.list_mes,self.list_ref))
        self.sel_mes.grid(row=r,column=1)


        r=r+1
        self.but_vis = Button(fl, text="Visualiser temporels", command=self.visu_tempo)
        self.but_vis.grid(row=r,column=2)


        self.is_tab_temp=0
        self.is_tab_spec=0
        self.is_tab_frf=0
        self.is_tab_coh=0


        fl.grid(row=1,sticky='ew');

        # Parametres du traitement du signal

        fp=Frame(self,relief='sunken', borderwidth=1)
        r=0 # row index

        Label(fp,text="Type de fenetre :",pady=5).grid(row=r,column=2)
        r=r+1
        self.radio_win = IntVar()
        windows=["Naturelle","Hanning","Hamming","Exponentielle","Utilisateur"]
        for win in range(len(windows)) :
           Radiobutton(fp, text=windows[win], variable=self.radio_win, value=win).grid(row=r,column=win)
        r=r+1
        Label(fp,text="Parametres (cas des fenetres Exponentielle et Utilisateur) :",
              pady=5).grid(row=r,column=0,columnspan=3)
        self.entry_win_par=Entry(fp,background='white',width=20)
        self.entry_win_par.grid(row=r,column=3,columnspan=2)

        r=r+1
        Label(fp,text="___________________").grid(row=r,column=2)
        r=r+1
        Label(fp,text=" ").grid(row=r,column=2)

        r=r+1
        Label(fp,text="Longueur :",pady=5).grid(row=r,column=0)
        longueur=[u"Durée","Points","Pourcent"]
        self.radio_long = IntVar()
        for lon in range(len(longueur)) :
           Radiobutton(fp, text=longueur[lon],variable=self.radio_long, value=lon).grid(row=r,column=lon+2)
        self.entry_long=Entry(fp,background='white',width=10)
        self.entry_long.grid(row=r,column=1)

        r=r+1
        Label(fp,text="Recouvrement :",pady=5).grid(row=r,column=0)
        self.radio_rec = IntVar()
        recouvr=[u"Durée","Points","Pourcent"]
        for rec in range(len(recouvr)) :
           Radiobutton(fp, text=recouvr[rec], variable=self.radio_rec, value=rec).grid(row=r,column=rec+2)
        self.entry_rec=Entry(fp,background='white',width=10)
        self.entry_rec.grid(row=r,column=1)

        r=r+1
        Label(fp,text="___________________").grid(row=r,column=2)
        r=r+1
        Label(fp,text=" ").grid(row=r,column=2)


        r=r+1
        self.view_spec = Button(fp, text="Interspectres", command=self.calc_intespec)
        self.view_spec.grid(row=r,column=1)

        self.view_spec = Button(fp, text="Transferts", command=self.calc_transfert)
        self.view_spec.grid(row=r,column=2)

        self.view_spec = Button(fp, text=u"Cohérence", command=self.calc_coherence)
        self.view_spec.grid(row=r,column=3)

        r=r+1
        self.radio_h1h2 = IntVar()
        Radiobutton(fp, text="H1", variable=self.radio_h1h2, value=0).grid(row=r,column=2)
        r=r+1
        Radiobutton(fp, text="H2", variable=self.radio_h1h2, value=1).grid(row=r,column=2)

        r=r+1

        fp.grid(row=2,column=0);

        # Frame de visualisation
        self.fv = Frame(self)
        self.fv = self.frame_visu('R')
        self.fv.grid(row=1,column=1,rowspan=2,sticky='n')
        


    def frame_visu(self,type_data='R') :

        if self.fv:
            # on vire la frame precedente
            le=len(self.fv.winfo_children())
            for i1 in range(le):
               self.fv.winfo_children()[0].destroy()
            self.fv.destroy()

        self.curve_list = None             # tableau de visu (utilise VisuSpectre, dans laquelle on peut avoir plusieurs listes cote a cote, d'ou l'utilisation d'une liste a une composante)
        self.var_visu_resu = StringVar()   # variable du choix de visu (non utilise pour calc_spec)
        self.label_visu = StringVar()      # variable pour le label de la colonne de visu
        self.radio_donnees = IntVar()      # visu reel, abs, imag, phase
        self.xlinlog = IntVar()            # axe x lin ou log
        self.ylinlog = IntVar()            # axe y lin ou log

        # on en refait une neuve        
        visu = VisuSpectre(self,nb_col=1,choix=None,
                           export=None,
                           type_data=type_data,
                           label_visu=self.label_visu)      # classe de visualisation des spectres

        return visu
        


    def visu_tempo(self) :

        #-- On teste les entrees
        if self.list_mes.size()==0 :
            self.mess.disp_mess(u"Pas de mesure sélectionnée pour la visualisation")
            return

        #-- Si OK, on continu
        if self.is_tab_temp :
           DETRUIRE(CONCEPT=_F(NOM=(self.tab_temp.obj,)), INFO=1)
        self.is_tab_temp=1

        self.list_mes.selection_set(0,self.list_mes.size())
        self.tab_temp=self.crea_tab_fonc()
                                
        self.list_mes.selection_clear(0,self.list_mes.size())

        self.fv = self.frame_visu('R')
        self.fv.grid(row=1,column=1,rowspan=2,sticky='n')

        #-- remise a jour de la liste pour la visu
##        self.curve_list.delete()
        self.coupl_ddl = zip(self.tab_temp.nume_ordr,self.tab_temp.nume_mes)
        lab=[]
        for nume_ordr,nume_mes in self.coupl_ddl:
            lab.append('Pt. '+str(nume_ordr)+' - Mes. '+str(nume_mes))
            
        self.curve_list.set_values(lab)
        self.label_visu.set("Echantillons :")
       


    def display_curve(self) :

        type_res=self.label_visu.get()
        ind_v = [kk[0] for kk in self.curve_list.get_selection()]
        v = [kk[1] for kk in self.curve_list.get_selection()]
        
        if not ind_v:
            self.mess.disp_mess( "Aucune courbe selectionnee !!")
            return
        try:
           ind_v = map(int, ind_v)
        except ValueError: pass

        tmp = [list(self.coupl_ddl[kk]) for kk in ind_v]
        coupl_ddl = [ list(kk)+[jj] for kk,jj in zip(tmp,v)]

        couleur=[4,15,2,1,10,6,7,8,9,11,12,13]
        #Pour xmgrace, les couleurs sont :
        # 0 blanc       1 noir    2 rouge   3 vert    4 bleu
        # 5 jaune       6 brun    7 gris    8 violet  9 cyan
        # 10 magenta    11 orange 12 marron 13 indigo 14 turquoise
        # 15 vert fonce
        nbc = len(couleur)
        l_coul = [couleur[i1 % nbc] for i1 in range(len(ind_v))]

        # selon le type de donnees extraites, parametres et legendes :
        data={'I':[self.Spec,'FONCTION_C','Fréquence','Hz','Inter-spectre','unite^2/Hz','Inter-spectre'],
              'T':[self.FRF,'FONCTION_C','Fréquence','Hz','Fonction_de_transfert',
                   'unite_mesure/unite_ref/Hz','Fonction de transfert'],
              'C':[self.Coh,'FONCTION_C','Fréquence','Hz','Cohérence',' ','Cohérence'],
              'E':[self.tab_temp,'FONCTION','Temps','s','Fonction_temporelle','unite_mesure',
                   'Fonction_temporelle']}

    
        courbes=[]
        legende=[]
        typ = type_res[0]
        inte_spec = data[typ][0]
        for nume_i,nume_j,leg in coupl_ddl :
            if typ == 'E' :
                # Fonction temporelle :
                __fonc_t = RECU_FONCTION(TABLE = self.tab_temp.obj,
                                         NOM_PARA_TABL='FONCTION',
                                         FILTRE = (_F(NOM_PARA='NUME_ORDRE_I',VALE_I=nume_i),
                                                   _F(NOM_PARA='NUME_MES',    VALE_I=nume_j),),)

                fonc_py = __fonc_t.convert('real')

            else:
                # Inter-spectres (IS, FRF, coherence)
                if nume_i == nume_j:
                    # la fonction extraite est reelle
                    __fonc_t = RECU_FONCTION(INTE_SPEC=inte_spec.obj,
                                             NUME_ORDRE_I=nume_i)
                    
                    fonc_py = __fonc_t.convert('real')
                  
                else:
                    # fonction complexe
                    __fonc_t = RECU_FONCTION(INTE_SPEC=inte_spec.obj,
                                             NUME_ORDRE_I=nume_i,
                                             NUME_ORDRE_J=nume_j)
                
                    fonc_py = __fonc_t.convert('complex')

            absc = fonc_py.vale_x
            ordo = fonc_py.vale_y
            
            DETRUIRE(CONCEPT=_F(NOM=(__fonc_t)), INFO=1)


            if self.radio_donnees.get()==0 :
               sig = [ kk.real for kk in ordo]
            elif self.radio_donnees.get()==2 :
               sig = [ kk.imag for kk in ordo]
            elif self.radio_donnees.get()==1 :
               sig = [abs(kk) for kk in ordo]
            elif self.radio_donnees.get()==3 :
               sig = [numpy.angle(kk) for kk in ordo]

            ech_y='LIN'
            if self.ylinlog.get()==1 :
               sig=numpy.log(numpy.absolute(sig)).tolist()
               #ech_y='LOG'

            ech_x='LIN'
            if self.xlinlog.get()==1 :
               # on enleve le point de frequence nulle
               freq = absc.tolist()
               if freq.count(0) :
                  absc=absc[1:]
                  sig=sig[1:]
               absc=numpy.log(numpy.absolute(absc)).tolist()
               #ech_x='LOG'

            legende.append(leg)
            courbes.append(sig)

        self.param_visu.visu_courbe(absc, courbes, l_coul,
                                    data[typ][6], legende, data[typ][2], data[typ][3],
                                    data[typ][4], data[typ][5])


    def calc_intespec(self) :

        #-- On teste les entrees

        test1=len(self.list_ref.curselection())
        test2=len(self.list_mes.curselection())


        if (test1+test2)==0 :
            self.mess.disp_mess(u"Sélectionner au moins une mesure et une référence")
            return


        #-- Si OK, on continu

        if self.is_tab_temp :
           DETRUIRE(CONCEPT=_F(NOM=(self.tab_temp.obj,)), INFO=1)
        if self.is_tab_spec :
           DETRUIRE(CONCEPT=_F(NOM=(self.Spec.obj)), INFO=1)


        ind_sel = self.list_mes.curselection()
        ind_tab = self.list_tab_cur
        try:
           ind_sel = map(int, ind_sel)
        except ValueError: pass

        self.Cur_Tab='I'

        self.tab_temp=self.crea_tab_fonc()
        self.is_tab_temp=1

        windows=["RECT","HANN","HAMM","EXPO","PART"]
        longueur=["LONGUEUR_DUREE","LONGUEUR_NB_PTS","LONGUEUR_POURCENT"]
        recouvrement=["RECOUVREMENT_DUREE","RECOUVREMENT_NB_PTS","RECOUVREMENT_POURCENT"]

        para_fene=self.entry_win_par.get()
        lon=self.entry_long.get()
        rec=self.entry_rec.get()

        mcfact_tabechant={}
        mcfact_tabechant.update({'NOM_TAB':self.tab_temp.obj})

        if len(lon)>0 :
            mcfact_tabechant.update({longueur[self.radio_long.get()]:float(self.entry_long.get())})
        else :
            mcfact_echant.update({'LONGUEUR_POURCENT':100.})

        if len(rec)>0 :
            mcfact_tabechant.update({recouvrement[self.radio_rec.get()]:float(self.entry_rec.get())})
        else :
            mcfact_tabechant.update({'RECOUVREMENT_POURCENT':0.})

        # Fenetrage
        mcfact_win={}
        if self.radio_win.get() <= 2 :
             mcfact_win = {'FENETRE':windows[self.radio_win.get()]}
        else :
             mcfact_win = {'DEFI_FENE':para_fene} 

        # Lancement de
        try:
            __Spec=CALC_SPEC(TAB_ECHANT=mcfact_tabechant,
                             INTERSPE=mcfact_win,)

        except EOFError, err :
            self.mess.disp_mess( "ERREUR DANS CALC_SPEC") 
            UTMESS('A','CALCESSAI0_8')
            return


        self.Spec=InterSpectre('Inter-spectre',__Spec,None,[],self.mess,None)
        self.is_tab_spec=1
        self.objects.update( "Spec", __Spec )

        #-- remise a jour de la liste pour la visu

        self.fv = self.frame_visu('C')
        self.fv.grid(row=1,column=1,rowspan=2,sticky='n')
##        self.curve_list.delete()

        self.coupl_ddl = self.Spec.extr_nume_ordr()

        lst = []
        for nume_i,nume_j in self.coupl_ddl:
           lst.append('Pt. ' + str(nume_i) + ' - Pt. ' + str(nume_j) )
        self.curve_list.set_values(lst)
        self.label_visu.set("Interspectres :")


    def calc_coherence(self) :

        #-- Teste des entrees
        test1=len(self.list_ref.curselection())
        test2=len(self.list_mes.curselection())

        if (test1*test2)==0 :
            self.mess.disp_mess(u"Sélectionner au moins une mesure et une référence")
            return

        #-- Si OK, on continu
        if self.is_tab_temp :
           DETRUIRE(CONCEPT=_F(NOM=(self.tab_temp.obj,)), INFO=1)
        if self.is_tab_coh :
           DETRUIRE(CONCEPT=_F(NOM=(self.Coh.obj)), INFO=1)

        ind_sel = self.list_ref.curselection()
        ind_tab = self.list_tab_cur
        try:
           ind_sel = map(int, ind_sel)
        except ValueError: pass

        self.Cur_Tab='C'

        self.tab_temp=self.crea_tab_fonc()
        self.is_tab_temp=1

        windows=["RECT","HANN","HAMM","EXPO","PART"]
        
        longueur=["LONGUEUR_DUREE","LONGUEUR_NB_PTS","LONGUEUR_POURCENT"]
        recouvrement=["RECOUVREMENT_DUREE","RECOUVREMENT_NB_PTS","RECOUVREMENT_POURCENT"]

        para_fene=self.entry_win_par.get()
        lon=self.entry_long.get()
        rec=self.entry_rec.get()

        mcfact_tabechant={}
        mcfact_tabechant.update({'NOM_TAB':self.tab_temp.obj})

        if len(lon)>0 :
            mcfact_tabechant.update({longueur[self.radio_long.get()]:float(self.entry_long.get())})
        else :
            mcfact_echant.update({'LONGUEUR_POURCENT':100.})

        if len(rec)>0 :
            mcfact_tabechant.update({recouvrement[self.radio_rec.get()]:float(self.entry_rec.get())})
        else :
            mcfact_tabechant.update({'RECOUVREMENT_POURCENT':0.})

        # recuperation des points de reference. Pour les transfert, une seule reference.        
        tmp = self.list_ref.get(0)
        if type(tmp) == type(''):
            ind_ref = int(tmp.split()[1])
        else:
            ind_ref = int(tmp[0].split()[1])

        #Fenetrage
        mcfact_win={}
        if self.radio_win.get() <= 2 :
             mcfact_win = {'REFER':ind_ref,
                           'FENETRE':windows[self.radio_win.get()]}
        else :
             mcfact_win = {'REFER':int(ind_ref),
                           'DEFI_FENE':para_fene}

        # Lancement de CALC_SPEC
        try:
            __Coh=CALC_SPEC(TAB_ECHANT=mcfact_tabechant,
                            TRANSFERT=mcfact_win,)

        except EOFError, err :
            self.mess.disp_mess( "ERREUR DANS CALC_SPEC")
            UTMESS('A','CALCESSAI0_8')
            return

    
        self.Coh=InterSpectre('Coherence',__Coh,None,[],self.mess,None)
        self.is_tab_coh=1

        #-- remise a jour de la liste pour la visu
        self.fv = self.frame_visu('R')
        self.fv.grid(row=1,column=1,rowspan=2,sticky='n')
##        self.curve_list.delete()

        self.coupl_ddl = self.Spec.extr_nume_ordr()

        lst = []
        for nume_i,nume_j in self.coupl_ddl:
           lst.append('Pt. ' + str(nume_i) + ' - Pt. ' + str(nume_j) )
        self.curve_list.set_values(lst)
        self.label_visu.set(u"Cohérences :")
        

    def calc_transfert(self) :

        #-- Test des entrees
        test1=len(self.list_ref.curselection())
        test2=len(self.list_mes.curselection())

        if (test1*test2)==0 :
            self.mess.disp_mess(u"Sélectionner au moins une mesure et une référence")
            return
        if test1 > 1:
            self.mess.disp_mess(u"Pour le calcul des fonctions de transfert, ne sélectionner qu'une seule référence !")
            return            

        #-- Si OK, on continu
        if self.is_tab_temp :
           DETRUIRE(CONCEPT=_F(NOM=(self.tab_temp.obj,)), INFO=1)
        if self.is_tab_frf :
           DETRUIRE(CONCEPT=_F(NOM=(self.FRF.obj,)), INFO=1)
        self.refresh_ltab()

        ind_sel = self.list_ref.curselection()
        ind_tab = self.list_tab_cur
        try:
           ind_sel = map(int, ind_sel)
        except ValueError: pass

        self.tab_temp=self.crea_tab_fonc()
        self.is_tab_temp=1

        windows=["RECT","HANN","HAMM","EXPO","PART"]
        longueur=["LONGUEUR_DUREE","LONGUEUR_NB_PTS","LONGUEUR_POURCENT"]
        recouvrement=["RECOUVREMENT_DUREE","RECOUVREMENT_NB_PTS","RECOUVREMENT_POURCENT"]

        para_fene=self.entry_win_par.get()
        lon=self.entry_long.get()
        rec=self.entry_rec.get()

        mcfact_tabechant={}
        mcfact_tabechant.update({'NOM_TAB':self.tab_temp.obj})
        
        if len(lon)>0 :
            mcfact_tabechant.update({longueur[self.radio_long.get()]:float(self.entry_long.get())})
        else :
            mcfact_echant.update({'LONGUEUR_POURCENT':100.})

        if len(rec)>0 :
            mcfact_tabechant.update({recouvrement[self.radio_rec.get()]:float(self.entry_rec.get())})
        else :
            mcfact_tabechant.update({'RECOUVREMENT_POURCENT':0.})

        # recuperation des points de reference. Pour les transfert, une seule reference.        
        tmp = self.list_ref.get(0)
        if type(tmp) == type(''):
            ind_ref = int(tmp.split()[1])
        else:
            ind_ref = int(tmp[0].split()[1])

        # Fenetrage
        mcfact_win={}
        if self.radio_win.get() <= 2 :
             mcfact_win = {'REFER':ind_ref,
                           'ESTIM':"H" + str(self.radio_h1h2.get()+1),
                           'FENETRE':windows[self.radio_win.get()]}
        else :
             mcfact_win = {'REFER':int(ind_ref),
                           'ESTIM':"H" + str(self.radio_h1h2.get()+1),
                           'DEFI_FENE':para_fene}            

        #-- Lancement de CALC_SPEC
        try:
            __FRF=CALC_SPEC(TAB_ECHANT=mcfact_tabechant,
                            TRANSFERT=mcfact_win)
        
        except EOFError, err :
            self.mess.disp_mess( "ERREUR DANS CALC_SPEC") 
            UTMESS('A','CALCESSAI0_8')
            return

        
        self.FRF=InterSpectre('FRF',__FRF,None,[],self.mess,None)
        self.is_tab_frf=1
        self.Cur_Tab='T'
        
        #-- remise a jour de la liste pour la visu
        self.fv = self.frame_visu('C')
        self.fv.grid(row=1,column=1,rowspan=2,sticky='n')

        self.coupl_ddl = self.FRF.extr_nume_ordr()

        lst = []
        for nume_i,nume_j in self.coupl_ddl:
            lst.append('Pt. ' +str(nume_j)+ ' / Pt. ' + str(nume_i))
        self.curve_list.set_values(lst)
        self.label_visu.set("Transferts (H" + str(self.radio_h1h2.get()+1) + ") :")



    def crea_tab_fonc(self) :

        from Cata.cata import CREA_TABLE
        # recuperation des numeros d'ordre et de reference

        ind_mes = self.list_mes.curselection()
        ind_tab = self.list_tab_cur
        try:
           ind_mes = map(int, ind_mes)
        except ValueError: pass

        # creation d'une table_fonction a partir des indices selectionnes
        keys=self.list_temp.keys()
        list_temp=self.list_temp[keys[ind_tab]]
        list_temp.extr_tempo()

        ind_mes = self.list_mes.curselection()
        ind_ref = self.list_ref.curselection()
        ind_tab = self.list_tab_cur
        try:
           ind_mes = map(int, ind_mes)
           ind_ref = map(int, ind_ref)
        except ValueError: pass

        # recuperation des points de mesure
        ind_mes_l=[]
        for i1 in range(len(ind_mes)) :
           if type(self.list_mes.get(ind_mes[i1],ind_mes[i1])) == type('') :
              lab=''+self.list_mes.get(ind_mes[i1],ind_mes[i1])
           else :
              lab=''+self.list_mes.get(ind_mes[i1],ind_mes[i1])[0]
           ind_mes_l.append(int(lab.split()[1]))

        # recuperation des points de reference
        ind_ref_l=[]
        for i1 in range(len(ind_ref)) :
           if type(self.list_ref.get(ind_ref[i1],ind_ref[i1])) == type('') :
              lab=''+self.list_ref.get(ind_ref[i1],ind_ref[i1])
           else :
              lab=''+self.list_ref.get(ind_ref[i1],ind_ref[i1])[0]
           ind_ref_l.append(int(lab.split()[1]))

        # construction de la liste unique des numeros d'ordre
        ind_sel=[]+ind_mes_l
        for i1 in range(len(ind_ref_l)) :
           if ind_sel.count(ind_ref_l[i1])==0 :
              ind_sel.append(ind_ref_l[i1])
        ind_sel.sort()

        # construction de la liste des indices des differents numeros d'ordre
        ind_new=[]
        for mes in ind_sel :
            ind_new.append(list_temp.nume_ordr.index(mes))
            for i1 in range(1,list_temp.nume_ordr.count(mes)) :
               ind_new.append(list_temp.nume_ordr[ind_new[-1]+1:].index(mes)+ind_new[-1]+1)

        new_nume_ordr=[list_temp.nume_ordr[i1] for i1 in ind_new]
        new_nume_mes=[list_temp.nume_mes[i1] for i1 in ind_new]
        new_nom_fonc=[list_temp.nom_fonc[i1] for i1 in ind_new]

        mcfact=[]
        mcfact.append(_F(PARA='NOM_CHAM'    ,LISTE_K='Rep_Temp'  ))
        mcfact.append(_F(PARA='NUME_ORDRE_I',LISTE_I=new_nume_ordr ))
        mcfact.append(_F(PARA='NUME_MES',LISTE_I=new_nume_mes ))
        mcfact.append(_F(PARA='FONCTION'  ,LISTE_K=new_nom_fonc  ))

        __tab_temp = CREA_TABLE(LISTE=mcfact,
                              TITRE = '',
                              TYPE_TABLE='TABLE_FONCTION')

        tempo = Tempo('__tab_temp',__tab_temp,0,None,None,[],self.mess,None)
        self.objects.update('__tab_temp',tempo)
        tempo.extr_tempo()
        self.refresh_ltab()

        return tempo

    def refresh_ltab(self):
        # rafraichissement des tables disponibles
        self.list_tab.delete(0,'end')
        self.objects.recup_objects()
        self.list_temp = self.objects.list_tempo
        for item in  self.list_temp.keys() : self.list_tab.insert('end',item)


    def refresh(self,list_tempo,list_tab,list_mes,list_ref) :

        # on enleve les definitions pre_existantes
        list_mes.delete(0,'end')
        list_ref.delete(0,'end')
        
        # on recupere le nom de la table_fonction selectionnee (1 seule)
        ind_tab = list_tab.curselection()
        if len(ind_tab) > 1:
            self.mess.disp_mess(u"Ne sélectionner qu'une seule table pour le calcul")
            return
        ind_tab = int(ind_tab[0])

        # pour suivre la table fonction sur laquelle on travaille (celle qui a ete exportee)
        if type(self) != type([]) : self.list_tab_cur=0+ind_tab
        keys=list_tempo.keys()
        list_temp=list_tempo[keys[ind_tab]]
        list_temp.extr_tempo()

        # index des numeros d'ordre
        out_o=self.crea_list_ind(list_temp.nume_ordr)
        l_ord=out_o[0]
        ind_ord=out_o[1]

        # index des numeros de mesure
        out_m=self.crea_list_ind(list_temp.nume_mes)
        l_mes=out_m[0]
        ind_mes=out_m[1]
        
        # on regoupe les differents points par paquet de mesures
        mes_group=[];
        for j1 in range(len(l_mes)) :
           mes_group.append([list_temp.nume_ordr[i1] for i1 in ind_mes[j1]])

        # on cherche les references communes a tous les points
        l_ref=[]+mes_group[0]
        ind_ref=[]+mes_group[0]
        for i1 in range(1,len(mes_group)) :
            for j1 in l_ref :
                if not mes_group[i1].count(j1) :
                   for k1 in range(l_ref.count(j1)) :
                      ind_ref[l_ref.index(j1)]=-1

        for i1 in range(ind_ref.count(-1)) :
            ind_ref.remove(-1)


        # on met a jour les listbox
        c=[]
        for i1 in range(l_ord[-1]+1) : c.append([])
        for mes in l_ord :
            c[mes].append(list_temp.nume_ordr.index(mes))
            for i1 in range(1,list_temp.nume_ordr.count(mes)) :
               c[mes].append(list_temp.nume_ordr[c[mes][-1]+1:].index(mes)+c[mes][-1]+1)
            M=[list_temp.nume_mes[i1] for i1 in c[mes]]
            list_mes.insert('end','Pt. ' + str(mes) + ' - Mes. ' + str(M))

        for ref in ind_ref :
            M=[list_temp.nume_mes[i1] for i1 in c[ref]]
            list_ref.insert('end','Pt. ' + str(ref) + ' - Mes. ' + str(M))



    def crea_list_ind(self,nume) :

        uu=[]+nume
        vv=[]+nume
        uu.sort()
        ind_ord=[]
        l_ord=[]
        while  len(uu) > 0 :
          l_ord.append(uu[0])
          tt=[];
          for i1 in range(uu.count(uu[0])) :
             tt.append(vv.index(uu[0]))
             vv[tt[-1]]=0
          ind_ord.append(tt)
          uu=uu[int(uu.count(uu[0])):]

        return l_ord, ind_ord


    def setup(self):
        "Utilise par outils_ihm.TabbedWindow"
        pass

    def teardown(self):
        "Utilise par utils_ihm.TabbedWindow"
        pass
