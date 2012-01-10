#@ MODIF ce_calc_spec Calc_essai  DATE 09/01/2012   AUTEUR REZETTE C.REZETTE 
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

# RESPONSABLE BODEL C.BODEL

import numpy

from Tkinter import Frame, Menubutton, Checkbutton, Menu, StringVar, IntVar
from Tkinter import Scrollbar, Label, Radiobutton, Button, Entry
from Tkinter import Checkbutton, Listbox, Toplevel, Message
import tkFont

from Accas import _F
from Cata.cata import DETRUIRE, CREA_TABLE, CALC_SPEC, RECU_FONCTION
from Calc_essai.cata_ce import InterSpectre, CreaTable, Tempo
from Calc_essai.outils_ihm import MultiList, DispFRFDialogue

from SD.sd_fonction import sd_fonction


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
        self.InterfaceCalcSpec(self)


    def InterfaceCalcSpec(self,root_tab) :

        # Cadre de titre

        ft=Frame(root_tab, borderwidth=4)

        lab = Label(ft,text="Traitement du signal",pady=5, font=self.font1 )
        lab.grid(row=0, column=0, columnspan = 2)
        ft.grid(row=0,sticky='ew')

        # definition des listes de temporels

        fl=Frame(root_tab,relief='sunken', borderwidth=1)
        r=0 # row index

        Label(fl,text="Tables disponibles :",pady=5).grid(row=r,column=0)
        Label(fl,text="Points de mesures :",pady=5).grid(row=r,column=2)
        Label(fl,text=u"Points de référence :",pady=5).grid(row=r,column=4)

        r=r+1

        # appeler refresh quand on change de selection dans list_tab #

        root_tab.list_tab = Listbox(fl,selectmode='browse',background='white')
        for item in  root_tab.list_temp.keys() : root_tab.list_tab.insert('end',item)
        root_tab.list_tab.grid(row=r,column=0)


        root_tab.list_mes = Listbox(fl,selectmode='extended',exportselection=False,background='white')
        root_tab.list_mes.grid(row=r,column=2)

        root_tab.list_ref = Listbox(fl,selectmode='extended',exportselection=False,background='white')
        root_tab.list_ref.grid(row=r,column=4)

        #-- pour s'assurer qu'on gerera bien la table qu'on vient d'exporter, et pas une autre.

        root_tab.list_tab_cur=-1
        root_tab.sel_mes = Button(fl, text="=>", command=lambda:
                                  self.refresh(root_tab.list_temp,root_tab.list_tab,
                                               root_tab.list_mes,root_tab.list_ref,root_tab))
        root_tab.sel_mes.grid(row=r,column=1)


        r=r+1
        root_tab.but_vis = Button(fl, text="Visualiser temporels", command=lambda:
                                  self.visu_tempo(root_tab))
        root_tab.but_vis.grid(row=r,column=2)


        root_tab.is_tab_temp=0
        root_tab.is_tab_spec=0
        root_tab.is_tab_frf=0
        root_tab.is_tab_coh=0



        fl.grid(row=1,sticky='ew');

        # Parametres du traitement du signal

        fp=Frame(root_tab,relief='sunken', borderwidth=1)
        r=0 # row index

        Label(fp,text="Type de fenetre :",pady=5).grid(row=r,column=2)
        r=r+1
        root_tab.radio_win = IntVar()
        windows=["Naturelle","Hanning","Hamming","Exponentielle","Utilisateur"]
        for win in range(len(windows)) :
           Radiobutton(fp, text=windows[win], variable=root_tab.radio_win, value=win).grid(row=r,column=win)
        r=r+1
        Label(fp,text="Parametres (cas des fenetres Exponentielle et Utilisateur) :",
              pady=5).grid(row=r,column=0,columnspan=3)
        root_tab.entry_win_par=Entry(fp,background='white',width=20)
        root_tab.entry_win_par.grid(row=r,column=3,columnspan=2)

        r=r+1
        Label(fp,text="___________________").grid(row=r,column=2)
        r=r+1
        Label(fp,text=" ").grid(row=r,column=2)

        r=r+1
        Label(fp,text="Longueur :",pady=5).grid(row=r,column=0)
        longueur=[u"Durée","Points","Pourcent"]
        root_tab.radio_long = IntVar()
        for lon in range(len(longueur)) :
           Radiobutton(fp, text=longueur[lon],variable=root_tab.radio_long, value=lon).grid(row=r,column=lon+2)
        root_tab.entry_long=Entry(fp,background='white',width=10)
        root_tab.entry_long.grid(row=r,column=1)

        r=r+1
        Label(fp,text="Recouvrement :",pady=5).grid(row=r,column=0)
        root_tab.radio_rec = IntVar()
        recouvr=[u"Durée","Points","Pourcent"]
        for rec in range(len(recouvr)) :
           Radiobutton(fp, text=recouvr[rec], variable=root_tab.radio_rec, value=rec).grid(row=r,column=rec+2)
        root_tab.entry_rec=Entry(fp,background='white',width=10)
        root_tab.entry_rec.grid(row=r,column=1)

        r=r+1
        Label(fp,text="___________________").grid(row=r,column=2)
        r=r+1
        Label(fp,text=" ").grid(row=r,column=2)


        r=r+1
        root_tab.view_spec = Button(fp, text="Interspectres", command=lambda: self.calc_intespec(root_tab))
        root_tab.view_spec.grid(row=r,column=1)

        root_tab.view_spec = Button(fp, text="Transferts", command=lambda: self.calc_transfert(root_tab))
        root_tab.view_spec.grid(row=r,column=2)

        root_tab.view_spec = Button(fp, text=u"Cohérence", command=lambda: self.calc_coherence(root_tab))
        root_tab.view_spec.grid(row=r,column=3)

        r=r+1
        root_tab.radio_h1h2 = IntVar()
        Radiobutton(fp, text="H1", variable=root_tab.radio_h1h2, value=0).grid(row=r,column=2)
        r=r+1
        Radiobutton(fp, text="H2", variable=root_tab.radio_h1h2, value=1).grid(row=r,column=2)

        r=r+1

        fp.grid(row=2,column=0);

        # Frame pour a visualisation

        root_tab.fv=Frame(root_tab,relief='sunken', borderwidth=1)
        self.frame_visu(root_tab,'R')

    def frame_visu(self,root_tab,type_data) :

        # on vire la frame precedente
        le=len(root_tab.fv.winfo_children())
        for i1 in range(le):
           root_tab.fv.winfo_children()[0].destroy()
        root_tab.fv.destroy()

        # on en refait une neuve

        root_tab.fv=Frame(root_tab,relief='sunken', borderwidth=1)
        r=0 # row index

        root_tab.label_visu=StringVar()
        Label(root_tab.fv,text="",textvariable=root_tab.label_visu,pady=5).grid(row=r,column=1)
        r=r+1

        root_tab.list_visu = Listbox(root_tab.fv,selectmode='extended',exportselection=False,background='white')
        root_tab.list_visu.grid(row=r,column=1)

        r=r+1
        donnees=["Reel ","Abs.","Imag.","Pha."]
        rc_r=[0,0,1,1]
        rc_c=[1,2,1,2]
        if type_data=='C' : #donnees complexes a visualiser
           Label(root_tab.fv,text=u"Données :",pady=5).grid(row=r,column=0)
           root_tab.radio_donnees = IntVar()
           for lon in range(len(donnees)) :
              Radiobutton(root_tab.fv, text=donnees[lon],
                          variable=root_tab.radio_donnees,
                          value=lon).grid(row=r+rc_r[lon],column=rc_c[lon])

           r=r+2
        else :
           Label(root_tab.fv,text=u"Données :",pady=5).grid(row=r,column=0)
           root_tab.radio_donnees = IntVar()
           for lon in range(2) :
              Radiobutton(root_tab.fv, text=donnees[lon],
                          variable=root_tab.radio_donnees,
                          value=lon).grid(row=r+rc_r[lon],column=rc_c[lon])

           r=r+1


        Label(root_tab.fv,text="Echelle X :",pady=5).grid(row=r,column=0)
        ech=[u"Linéaire","Logarithmique"]
        root_tab.radio_xlinlog = IntVar()
        for lon in range(len(ech)) :
           Radiobutton(root_tab.fv, text=ech[lon],variable=root_tab.radio_xlinlog, value=lon).grid(row=r,column=lon+1)

        r=r+1
        Label(root_tab.fv,text="Echelle Y :",pady=5).grid(row=r,column=0)
        root_tab.radio_ylinlog = IntVar()
        for lon in range(len(ech)) :
           Radiobutton(root_tab.fv, text=ech[lon],variable=root_tab.radio_ylinlog, value=lon).grid(row=r,column=lon+1)

        r=r+1
        root_tab.disp_mes = Button(root_tab.fv, text="Visualiser", command=lambda: self.display_mes(root_tab))
        root_tab.disp_mes.grid(row=r,column=1)

        root_tab.fv.grid(row=1,column=1,rowspan=2,sticky='n')

    def visu_tempo(self,root_tab) :

        #-- On teste les entrees


        if root_tab.list_mes.size()==0 :
              top = Toplevel()
              pos="200x100+" + str(top.winfo_screenwidth()/2-100) + '+'
              pos=pos + str(top.winfo_screenheight()/2-50)
              top.geometry(pos)
              top.title("Attention!")
              msg = Message(top, text="Il n'y a pas de mesure disponible pour la visualisation",
                            width=180,justify='center')
              msg.grid(row=0,column=0)
              button = Button(top, text="OK", command=top.destroy)
              button.grid(row=1,column=0)
              return


        #-- Si OK, on continu


        if root_tab.is_tab_temp :
           DETRUIRE(CONCEPT=_F(NOM=(root_tab.tab_temp,)), INFO=1)
        root_tab.is_tab_temp=1

        root_tab.list_mes.selection_set(0,root_tab.list_mes.size())
        root_tab.tab_temp=self.crea_tab_fonc(root_tab)
        root_tab.list_mes.selection_clear(0,root_tab.list_mes.size())

        self.frame_visu(root_tab,'R')

        #-- remise a jour de la liste pour la visu

        root_tab.list_visu.delete(0,'end')

        for i1 in range(root_tab.list_mes.size()) :
           if type(root_tab.list_mes.get(i1,i1)) == type('') :
              lab=''+root_tab.list_mes.get(i1,i1)
           else :
              lab=''+root_tab.list_ref.get(i1,i1)[0]
           root_tab.list_visu.insert('end',lab)

        root_tab.label_visu.set("Echantillons :")



    def display_mes(self,root_tab) :
        from Cata.cata import IMPR_FONCTION

        type_res=root_tab.label_visu.get()

        ind_v = root_tab.list_visu.curselection()
        if not ind_v:
            root_tab.mess.disp_mess( "Aucune courbe selectionnee !!")
            return
        try:
           ind_v = map(int, ind_v)
        except ValueError: pass
        #ind_visu=ind_v[0]

        ind_visu=[ind_v[i1] for i1 in range(len(ind_v))]

        if type_res[0]=='I' :
           tab_py=root_tab.Spec.EXTR_TABLE()
           nom_fonc=tab_py['FONCTION_C'].values()['FONCTION_C']
        if type_res[0]=='T' :
           tab_py=root_tab.FRF.EXTR_TABLE()
           nom_fonc=tab_py['FONCTION_C'].values()['FONCTION_C']
        if type_res[0]=='C' :
           tab_py=root_tab.Coh.EXTR_TABLE()
           nom_fonc=tab_py['FONCTION_C'].values()['FONCTION_C']
        if type_res[0]=='E' :
           tab_py=root_tab.tab_temp.EXTR_TABLE()
           nom_fonc=tab_py['FONCTION'].values()['FONCTION']

        couleur=[4,15,2,1,10,6,7,8,9,11,12,13]
        #Pour xmgrace, les couleurs sont :
        # 0 blanc       1 noir    2 rouge   3 vert    4 bleu
        # 5 jaune       6 brun    7 gris    8 violet  9 cyan
        # 10 magenta    11 orange 12 marron 13 indigo 14 turquoise
        # 15 vert fonce


        compt_coul=0
        courbes=[]
        legende=[]
        for i1 in ind_visu :
           if type_res[0]=='I' :
              fonc_t = RECU_FONCTION(TABLE=root_tab.Spec,
                                     NOM_PARA_TABL = 'FONCTION_C',
                                     FILTRE=_F(NOM_PARA='FONCTION_C',
                                     VALE_K = nom_fonc[i1],),
                                    )
           if type_res[0]=='T' :
              fonc_t = RECU_FONCTION(TABLE=root_tab.FRF,
                                     NOM_PARA_TABL = 'FONCTION_C',
                                     FILTRE=_F(NOM_PARA='FONCTION_C',
                                     VALE_K = nom_fonc[i1],),
                                    )
           if type_res[0]=='C' :
              fonc_t = RECU_FONCTION(TABLE=root_tab.Coh,
                                     NOM_PARA_TABL = 'FONCTION_C',
                                     FILTRE=_F(NOM_PARA='FONCTION_C',
                                     VALE_K = nom_fonc[i1],),
                                    )
           if type_res[0]=='E' :
              fonc_t = RECU_FONCTION(TABLE=root_tab.tab_temp,
                                     NOM_PARA_TABL = 'FONCTION',
                                     FILTRE=_F(NOM_PARA='FONCTION',
                                     VALE_K = nom_fonc[i1],),
                                    )


           valeurs=fonc_t.sdj.VALE.get()

           DETRUIRE(CONCEPT=_F(NOM=(fonc_t)), INFO=1)

           if type_res[0]=='E' :
              l_s=len(valeurs)
              freq=[valeurs[j1] for j1 in range(0,l_s/2)]
              p_reel=[valeurs[j1] for j1 in range(l_s/2,l_s)]
              p_imag=[0 for j1 in range(0,l_s/2)]
           else :
              l_s=len(valeurs)
              freq=[valeurs[j1] for j1 in range(0,l_s/3)]
              p_reel=[valeurs[j1] for j1 in range(l_s/3,l_s,2)]
              p_imag=[valeurs[j1] for j1 in range(l_s/3+1,l_s,2)]


           if root_tab.radio_donnees.get()==0 :
              sig=p_reel
           elif root_tab.radio_donnees.get()==2 :
              sig=p_imag
           elif root_tab.radio_donnees.get()==1 :
              sig=numpy.multiply(p_reel,p_reel) + numpy.multiply(p_imag,p_imag)
              sig=numpy.sqrt(sig)
              epsilon=2.22e-16
              sig=sig+epsilon
              sig=sig.tolist()
           elif root_tab.radio_donnees.get()==3 :
              sig=numpy.arctan(numpy.divide(p_imag,p_reel)).tolist()

           ech_y='LIN'
           if root_tab.radio_ylinlog.get()==1 :
              sig=numpy.log(numpy.absolute(sig)).tolist()
              #ech_y='LOG'

           ech_x='LIN'
           if root_tab.radio_xlinlog.get()==1 :
              # on enleve le point de frequence nulle
              if freq.count(0) :
                 freq=freq[1,:]
                 sig=sdisplay_mesig[1:]
              freq=numpy.log(numpy.absolute(freq)).tolist()
              #ech_x='LOG'


           if type(root_tab.list_visu.get(i1,i1)) == type('') :
              leg=''+root_tab.list_visu.get(i1,i1)
           else :
              leg=''+root_tab.list_visu.get(i1,i1)[0]
           legende.append(leg)
           courbes.append(sig)
           compt_coul=compt_coul+1

        #-- Ca serait bien que les couleurs soient dispos...

        self.param_visu.visu_courbe(freq, courbes, [couleur[i1] for i1 in range(len(ind_visu))],
                                    legende, 'LIN', 'LIN')


    def calc_intespec(self,root_tab) :

        #-- On teste les entrees

        test1=len(root_tab.list_ref.curselection())*root_tab.list_ref.size()
        test2=len(root_tab.list_mes.curselection())*root_tab.list_mes.size()


        if (test1+test2)==0 :
              top = Toplevel()
              pos="200x100+" + str(top.winfo_screenwidth()/2-100) + '+'
              pos=pos + str(top.winfo_screenheight()/2-50)
              top.geometry(pos)
              top.title("Attention!")
              msg = Message(top, text="Il n'y a pas de mesure disponible pour le calcul",
                            width=180,justify='center')
              msg.grid(row=0,column=0)
              button = Button(top, text="OK", command=top.destroy)
              button.grid(row=1,column=0)
              return


        #-- Si OK, on continu

        if root_tab.is_tab_temp :
           DETRUIRE(CONCEPT=_F(NOM=(root_tab.tab_temp,)), INFO=1)
        if root_tab.is_tab_spec :
           DETRUIRE(CONCEPT=_F(NOM=(root_tab.Spec)), INFO=1)


        ind_sel = root_tab.list_mes.curselection()
        ind_tab = root_tab.list_tab_cur
        try:
           ind_sel = map(int, ind_sel)
        except ValueError: pass


        root_tab.is_tab_temp=1
        root_tab.is_tab_spec=1
        root_tab.Cur_Tab='I'

        root_tab.tab_temp=self.crea_tab_fonc(root_tab)

        windows=["RECT","HANN","HAMM","EXPO","PART"]
        longueur=["LONGUEUR_DUREE","LONGUEUR_NB_PTS","LONGUEUR_POURCENT"]
        recouvrement=["RECOUVREMENT_DUREE","RECOUVREMENT_NB_PTS","RECOUVREMENT_POURCENT"]


        para_fene=root_tab.entry_win_par.get()
        lon=root_tab.entry_long.get()
        rec=root_tab.entry_rec.get()

        mcfact_tabechant=[]

        mcfact_tabechant.append(_F(NOM_TAB=root_tab.tab_temp))

        if len(lon)>0 :
           st="mcfact_tabechant.append(_F(" + longueur[root_tab.radio_long.get()] + "="
           st=st + root_tab.entry_long.get() + "))"
           exec(st)
        else :
           mcfact_echant.append(_F(LONGUEUR_POURCENT=100.))

        if len(rec)>0 :
           st="mcfact_tabechant.append(_F(" + recouvrement[root_tab.radio_rec.get()] + "="
           st=st + root_tab.entry_rec.get() + "))"
           exec(st)
        else :
           mcfact_tabechant.append(_F(RECOUVREMENT_POURCENT=0.))

        mcfact_win=[]
        if root_tab.radio_win.get() <= 2 :
           st="mcfact_win.append(_F(FENETRE=" + "'" + windows[root_tab.radio_win.get()] + "'))"
           exec(st)
        else :
           st="mcfact_win.append(_F(FENETRE=" + "'" + windows[root_tab.radio_win.get()] + "',"
           st=st+ "DEFI_FENE=" + para_fene + "),)"
           exec(st)

        Spec=CALC_SPEC(TAB_ECHANT=mcfact_tabechant,
                       INTERSPE=mcfact_win,
                      )
        root_tab.Spec=Spec

        root_tab.objects.update( "Spec", Spec )

        #-- remise a jour de la liste pour la visu

        self.frame_visu(root_tab,'C')

        root_tab.list_visu.delete(0,'end')

        tab_py=root_tab.Spec.EXTR_TABLE()
        nume_i=tab_py['NUME_ORDRE_I'].values()['NUME_ORDRE_I']
        nume_j=tab_py['NUME_ORDRE_J'].values()['NUME_ORDRE_J']


        for i1 in range(len(nume_i)) :
           root_tab.list_visu.insert('end','Pt. ' + str(nume_i[i1]) + ' - Pt. ' + str(nume_j[i1]) )

        root_tab.label_visu.set("Interspectres :")


    def calc_coherence(self,root_tab) :

        #-- On teste les entrees

        test1=len(root_tab.list_ref.curselection())*root_tab.list_ref.size()
        test2=len(root_tab.list_mes.curselection())*root_tab.list_mes.size()

        if (test1*test2)==0 :
              top = Toplevel()
              pos="200x100+" + str(top.winfo_screenwidth()/2-100) + '+'
              pos=pos + str(top.winfo_screenheight()/2-50)
              top.geometry(pos)
              top.title("Attention!")
              msg = Message(top, text="Il n'y a pas de mesure de reference disponible",
                            width=180,justify='center')
              msg.grid(row=0,column=0)
              button = Button(top, text="OK", command=top.destroy)
              button.grid(row=1,column=0)
              return


        #-- Si OK, on continu

        if root_tab.is_tab_temp :
           DETRUIRE(CONCEPT=_F(NOM=(root_tab.tab_temp,)), INFO=1)
        if root_tab.is_tab_coh :
           DETRUIRE(CONCEPT=_F(NOM=(root_tab.Coh,)), INFO=1)

        ind_sel = root_tab.list_ref.curselection()
        ind_tab = root_tab.list_tab_cur
        try:
           ind_sel = map(int, ind_sel)
        except ValueError: pass

        root_tab.is_tab_temp=1
        root_tab.is_tab_coh=1
        root_tab.Cur_Tab='C'

        root_tab.tab_temp=self.crea_tab_fonc(root_tab)

        windows=["RECT","HANN","HAMM","EXPO","PART"]
        
        longueur=["LONGUEUR_DUREE","LONGUEUR_NB_PTS","LONGUEUR_POURCENT"]
        recouvrement=["RECOUVREMENT_DUREE","RECOUVREMENT_NB_PTS","RECOUVREMENT_POURCENT"]


        para_fene=root_tab.entry_win_par.get()
        lon=root_tab.entry_long.get()
        rec=root_tab.entry_rec.get()

        mcfact_tabechant=[]
        mcfact_tabechant.append(_F(NOM_TAB=root_tab.tab_temp))

        if len(lon)>0 :
           st="mcfact_tabechant.append(_F(" + longueur[root_tab.radio_long.get()] + "="
           st=st + root_tab.entry_long.get() + "))"
           exec(st)
        else :
           mcfact_echant.append(_F(LONGUEUR_POURCENT=100.))

        if len(rec)>0 :
           st="mcfact_tabechant.append(_F(" + recouvrement[root_tab.radio_rec.get()] + "="
           st=st + root_tab.entry_rec.get() + "))"
           exec(st)
        else :
           mcfact_tabechant.append(_F(RECOUVREMENT_POURCENT=0.))

        # recuperation des points de reference
        ind_ref='['
        for i1 in range(len(ind_sel)) :
           if type(root_tab.list_ref.get(ind_sel[i1],ind_sel[i1])) == type('') :
              lab=''+root_tab.list_ref.get(ind_sel[i1],ind_sel[i1])
           else :
              lab=''+root_tab.list_ref.get(ind_sel[i1],ind_sel[i1])[0]
           lab=lab[4:]
           lab=lab[:lab.find('M')-2]
           ind_ref=ind_ref + lab +','

        ind_ref=ind_ref + '],'


        mcfact_win=[]
        if root_tab.radio_win.get() <= 2 :
           st="mcfact_win.append(_F(ESTIM=" + "'" + "CO" + "',"
           st=st + "REFER=" + ind_ref
           st=st + "FENETRE=" + "'" + windows[root_tab.radio_win.get()] + "'))"
           exec(st)
        else :
           st="mcfact_win.append(_F(ESTIM=" + "'" + "CO" + "',"
           st=st + "REFER=" + ind_ref
           st=st + "FENETRE=" + "'" + windows[root_tab.radio_win.get()] + "',"
           st=st+ "DEFI_FENE=" + para_fene + "))"
           exec(st)

        Coh=CALC_SPEC(TAB_ECHANT=mcfact_tabechant,
                      TRANSFERT=mcfact_win,
                      )
        root_tab.Coh=Coh

        #-- remise a jour de la liste pour la visu

        self.frame_visu(root_tab,'R')

        root_tab.list_visu.delete(0,'end')

        tab_py=root_tab.Coh.EXTR_TABLE()
        nume_i=tab_py['NUME_ORDRE_I'].values()['NUME_ORDRE_I']
        nume_j=tab_py['NUME_ORDRE_J'].values()['NUME_ORDRE_J']


        for i1 in range(len(nume_i)) :
           root_tab.list_visu.insert('end','Pt. ' + str(nume_j[i1]) + ' / Pt. ' + str(nume_i[i1]) )

        root_tab.label_visu.set("Coherences :")


    def calc_transfert(self,root_tab) :

        #-- On teste les entrees

        test1=len(root_tab.list_ref.curselection())*root_tab.list_ref.size()
        test2=len(root_tab.list_mes.curselection())*root_tab.list_mes.size()

        if (test1*test2)==0 :
              top = Toplevel()
              pos="200x100+" + str(top.winfo_screenwidth()/2-100) + '+'
              pos=pos + str(top.winfo_screenheight()/2-50)
              top.geometry(pos)
              top.title("Attention!")
              msg = Message(top, text="Il n'y a pas de mesure de reference disponible",
                            width=180,justify='center')
              msg.grid(row=0,column=0)
              button = Button(top, text="OK", command=top.destroy)
              button.grid(row=1,column=0)
              return


        #-- Si OK, on continu

        if root_tab.is_tab_temp :
           DETRUIRE(CONCEPT=_F(NOM=(root_tab.tab_temp,)), INFO=1)
        if root_tab.is_tab_frf :
           DETRUIRE(CONCEPT=_F(NOM=(root_tab.FRF,)), INFO=1)

        ind_sel = root_tab.list_ref.curselection()
        ind_tab = root_tab.list_tab_cur
        try:
           ind_sel = map(int, ind_sel)
        except ValueError: pass

        root_tab.is_tab_temp=1
        root_tab.is_tab_frf=1
        root_tab.Cur_Tab='T'

        root_tab.tab_temp=self.crea_tab_fonc(root_tab)

        windows=["RECT","HANN","HAMM","EXPO","PART"]
        
        longueur=["LONGUEUR_DUREE","LONGUEUR_NB_PTS","LONGUEUR_POURCENT"]
        recouvrement=["RECOUVREMENT_DUREE","RECOUVREMENT_NB_PTS","RECOUVREMENT_POURCENT"]


        para_fene=root_tab.entry_win_par.get()
        lon=root_tab.entry_long.get()
        rec=root_tab.entry_rec.get()

        mcfact_tabechant=[]
        mcfact_tabechant.append(_F(NOM_TAB=root_tab.tab_temp))

        if len(lon)>0 :
           st="mcfact_tabechant.append(_F(" + longueur[root_tab.radio_long.get()] + "="
           st=st + root_tab.entry_long.get() + "))"
           exec(st)
        else :
           mcfact_echant.append(_F(LONGUEUR_POURCENT=100.))

        if len(rec)>0 :
           st="mcfact_tabechant.append(_F(" + recouvrement[root_tab.radio_rec.get()] + "="
           st=st + root_tab.entry_rec.get() + "))"
           exec(st)
        else :
           mcfact_tabechant.append(_F(RECOUVREMENT_POURCENT=0.))

        # recuperation des points de reference

        ind_ref='['
        for i1 in range(len(ind_sel)) :
           if type(root_tab.list_ref.get(ind_sel[i1],ind_sel[i1])) == type('') :
              lab=''+root_tab.list_ref.get(ind_sel[i1],ind_sel[i1])
           else :
              lab=''+root_tab.list_ref.get(ind_sel[i1],ind_sel[i1])[0]
           lab=lab[4:]
           lab=lab[:lab.find('M')-2]
           ind_ref=ind_ref + lab +','

        ind_ref=ind_ref + '],'


        mcfact_long=[]
        if len(lon)>0 :
           st="mcfact_long.append(_F(" + longueur[root_tab.radio_long.get()] + "="
           st=st + root_tab.entry_long.get() + "))"
           exec(st)
        else :
           mcfact_long.append(_F(POURCENT=100))

        mcfact_rec=[]
        if len(rec)>0 :
           st="mcfact_rec.append(_F(" + longueur[root_tab.radio_rec.get()] + "="
           st=st + root_tab.entry_rec.get() + "))"
           exec(st)
        else :
           mcfact_rec.append(_F(POURCENT=0))

        mcfact_win=[]
        if root_tab.radio_win.get() <= 2 :
           st="mcfact_win.append(_F(ESTIM=" + "'" + "H" + str(root_tab.radio_h1h2.get()+1) + "',"
           st=st + "REFER=" + ind_ref
           st=st + "FENETRE=" + "'" + windows[root_tab.radio_win.get()] + "'))"
           exec(st)
        else :
           st="mcfact_win.append(_F(ESTIM=" + "'" + "H" + str(root_tab.radio_h1h2.get()+1) + "',"
           st=st + "REFER=" + ind_ref
           st=st + "FENETRE=" + "'" + windows[root_tab.radio_win.get()] + "',"
           st=st+ "DEFI_FENE=" + para_fene + "))"
           exec(st)

        FRF=CALC_SPEC(TAB_ECHANT=mcfact_tabechant,
                      TRANSFERT=mcfact_win,
                      )
        root_tab.FRF=FRF
        #-- remise a jour de la liste pour la visu

        self.frame_visu(root_tab,'C')

        root_tab.list_visu.delete(0,'end')

        tab_py=root_tab.FRF.EXTR_TABLE()
        nume_i=tab_py['NUME_ORDRE_I'].values()['NUME_ORDRE_I']
        nume_j=tab_py['NUME_ORDRE_J'].values()['NUME_ORDRE_J']


        for i1 in range(len(nume_i)) :
           root_tab.list_visu.insert('end','Pt. ' + str(nume_j[i1]) + ' / Pt. ' + str(nume_i[i1]) )

        root_tab.label_visu.set("Transferts (H" + str(root_tab.radio_h1h2.get()+1) + ") :")

    def crea_tab_fonc(self,root_tab) :

        from Cata.cata import CREA_TABLE
        # recuperation des numeros d'ordre et de reference

        ind_mes = root_tab.list_mes.curselection()
        ind_tab = root_tab.list_tab_cur
        try:
           ind_mes = map(int, ind_mes)
        except ValueError: pass

        # creation d'une table_fonction a partir des indices selectionnes

        keys=root_tab.list_temp.keys()
        list_temp=root_tab.list_temp[keys[ind_tab]]
        list_temp.extr_tempo()

        ind_mes = root_tab.list_mes.curselection()
        ind_ref = root_tab.list_ref.curselection()
        ind_tab = root_tab.list_tab_cur
        try:
           ind_mes = map(int, ind_mes)
           ind_ref = map(int, ind_ref)

        except ValueError: pass


        # recuperation des points de mesure

        ind_mes_l=[]
        for i1 in range(len(ind_mes)) :
           if type(root_tab.list_mes.get(ind_mes[i1],ind_mes[i1])) == type('') :
              lab=''+root_tab.list_mes.get(ind_mes[i1],ind_mes[i1])
           else :
              lab=''+root_tab.list_mes.get(ind_mes[i1],ind_mes[i1])[0]

           #lab=''+root_tab.list_mes.get(ind_mes[i1],ind_mes[i1])
           lab=lab[4:]
           lab=lab[:lab.find('M')-2]
           ind_mes_l.append(int(lab))

        # recuperation des points de reference

        ind_ref_l=[]
        for i1 in range(len(ind_ref)) :
           if type(root_tab.list_ref.get(ind_ref[i1],ind_ref[i1])) == type('') :
              lab=''+root_tab.list_ref.get(ind_ref[i1],ind_ref[i1])
           else :
              lab=''+root_tab.list_ref.get(ind_ref[i1],ind_ref[i1])[0]

           lab=lab[4:]
           lab=lab[:lab.find('M')-2]
           ind_ref_l.append(int(lab))

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

        tab_temp = CREA_TABLE(LISTE=mcfact,
                              TITRE = '',
                              TYPE_TABLE='TABLE_FONCTION')

        return tab_temp

    def refresh(self,list_tempo,list_tab,list_mes,list_ref,root_tab) :

        # on enleve les definitions pre_existantes

        list_mes.delete(0,'end')
        list_ref.delete(0,'end')

        # on recupere le nom de la table_fonction selectionnee

        ind_tab = list_tab.curselection()
        try:
           ind_tab = map(int, ind_tab)
        except ValueError: pass


        # on va chercher les numeros d'ordre et de mesure pour chaque fonction
        ind_tab=ind_tab[0]


        # pour suivre la table fonction sur laquelle on travaille (celle qui a ete exportee)
        if type(root_tab) != type([]) : root_tab.list_tab_cur=0+ind_tab


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


