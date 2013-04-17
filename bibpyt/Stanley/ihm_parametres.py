#@ MODIF ihm_parametres Stanley  DATE 16/04/2013   AUTEUR ASSIRE A.ASSIRE 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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

#  OBJETS GRAPHIQUES TK DE L'IHM DES PARAMETRES



# Utilisation de Tix ou Tkinter / Pmw
use_Pmw = False
use_Tix = False
use_Tk  = True
#import Tix as Tk
import Tkinter as Tk

try:
   import Pmw
   use_Pmw = True
except:
   pass


import types, tkMessageBox
import ihm_parametres as objets


# ==============================================================================

class AFFICHAGE_PARAMETRES :

  """
    Fenetre d'affichage et de modification des parametres.
  """

  def __init__(self, master, dliste_section, dparam, para, aide, titre=_(u"Affichage"), largeur=800, hauteur=660):

    self.master         = master
    self.dliste_section = dliste_section
    self.dparam         = dparam
    self.aide           = aide
    self.para           = para
    self.largeur        = largeur
    self.hauteur        = hauteur

    # Objet Fenetre Tk
    self.fenetre2 = Tk.Toplevel(self.master)
    self.fenetre2.title(titre)
    self.fenetre2.geometry(str(largeur)+"x"+str(hauteur)+"+50+50")
    self.fenetre2.resizable(width='no', height='no')

    # Objet bulle d'aide
    if use_Tix:
#       self.bulle = objets.BULLE_TIX(frame=self.fenetre2)
       # Objet bulle d'aide
       self.bulle = Tk.Balloon(self.fenetre2)
       self.bulle.configure(bg='blue', borderwidth=1)
    elif use_Pmw:
       try:
          Pmw.initialise(self.fenetre2)
          self.bulle = objets.BULLE_PMW(frame=self.fenetre2)
       except:
          self.bulle = None
    else:
       self.bulle = None

    # Fontes
    self.fonte=self.para['PARAMETRES']['fonte']

    # Parametre de sortie
    self.nouveau_para = None

    # Mode courant
    self.old_mode_graphique = None
    self.old_mode           = None

    # Variables Tk
    self.dvar = {}
    for section in self.dliste_section.keys():
       self.dvar[section] = {}
    for cle in self.dparam.keys():
       section = self.dparam[cle]['section']
       if type(self.dparam[cle]['val'])==types.IntType:  self.dvar[section][cle] = Tk.IntVar()
       else:                                             self.dvar[section][cle] = Tk.StringVar()
       self.dvar[section][cle].set( self.para[section][cle] )


    # Frames
#     frame = Tk.Frame(self.fenetre2, bd=1, relief='sunken' )
#     frame.place(in_=self.fenetre2,x=0,y=0,anchor='nw',width=largeur, height=hauteur)

    #     ----------
    #    |    1    |
    #    ----------
    #   | 2  | 3  |
    #   ----------
    #  | 4  | 5  |
    #  ----------

    x_1 = largeur*0.25
    y_1 = 0
    largeur_1 = largeur*0.5
    hauteur_1 = 70
    frame_1 = Tk.Frame(self.fenetre2, bd=0, relief='flat' )
    frame_1.place(in_=self.fenetre2,x=x_1,y=y_1,anchor='nw',width=largeur_1, height=hauteur_1)

    x_2 = 0
    y_2 = hauteur_1
    largeur_2 = largeur*0.5
#    hauteur_2 = 340
    hauteur_2 = 370
    frame_2 = Tk.Frame(self.fenetre2, bd=0, relief='flat' )
    frame_2.place(in_=self.fenetre2,x=x_2,y=y_2,anchor='nw',width=largeur_2, height=hauteur_2)

    x_3 = largeur_2
    y_3 = hauteur_1
    largeur_3 = largeur*0.5
    hauteur_3 = hauteur_2
    frame_3 = Tk.Frame(self.fenetre2, bd=0, relief='flat' )
    frame_3.place(in_=self.fenetre2,x=x_3,y=y_3,anchor='nw',width=largeur_3, height=hauteur_3)

    x_4 = 0
    y_4 = hauteur_1 + hauteur_2
    largeur_4 = largeur*0.5
    hauteur_4 = hauteur - hauteur_1 - hauteur_2
    frame_4 = Tk.Frame(self.fenetre2, bd=0, relief='flat' )
    frame_4.place(in_=self.fenetre2,x=x_4,y=y_4,anchor='nw',width=largeur_4, height=hauteur_4)

    x_5 = largeur_4
    y_5 = hauteur_1 + hauteur_2
    largeur_5 = largeur*0.5
    hauteur_5 = hauteur - hauteur_1 - hauteur_3
    frame_5 = Tk.Frame(self.fenetre2, bd=0, relief='flat' )
    frame_5.place(in_=self.fenetre2,x=x_5,y=y_5,anchor='nw',width=largeur_5, height=hauteur_5)


    # Dictionnaires des objets Tk
    self.titre = {}
    self.label = {}
    self.objet = {}

    # Liste des cle par section
    self.liste_cle = {}

    # Liste des sections
    self.liste_section = [ ['MODE_GRAPHIQUE', frame_1, 10], ['CONFIG', frame_2, 10], ['PARAMETRES', frame_4, 10], ['VISUALISATION', frame_5, 10] ]

    # construction des objets Tk
    self.Construction_Objets(init=True)

#    # Canvas
#    self.objet_canvas = objets.CANVAS(frame=frame_3, x=10, y=30, h=hauteur_3-40, l=largeur_3-40, anchor='nw')

    # Aide
    self.case_aide = objets.CASE_TEXTE(frame=frame_3, x=10, y=30, h=hauteur_3-40, l=largeur_3-40, params=None, fonte=self.fonte)

    # Boutons
    bullemsg = _(u"Annuler")
    bouton_annuler = objets.BUTTON(frame_4, x=largeur_4*0.6, y=hauteur_4 - 50, txt=_(u"Annuler"), command=self.Quitter, width=10, fonte=self.fonte, background="grey", bulle=self.bulle, bullemsg=bullemsg)

    bullemsg = _(u"Valider les parametres et sortir")
    bouton_ok      = objets.BUTTON(frame_5, x=largeur_5*0.1, y=hauteur_5 - 50, txt=_(u"OK"), command=self.Renvoi_Parametres, width=10, fonte=self.fonte, background="grey", bulle=self.bulle, bullemsg=bullemsg)

#     bullemsg = _(u"")
#     bouton_voir      = objets.BUTTON(frame_5, x=largeur_5*0.5, y=hauteur_5 - 50, txt=_(u"Voir"), command=self.Voir, width=10, fonte=self.fonte, background="grey", bulle=self.bulle, bullemsg=bullemsg)

    # Affiche l'aide
    self.Raffraichir_Aide()

    self.after_id = self.fenetre2.after(50, self.Scan)
    self.fenetre2.mainloop()


  def Raffraichir_Aide(self):
    """
       Raffraichir la case de l'aide
    """

    mode_graphique = self.dvar['MODE_GRAPHIQUE']['mode_graphique'].get()
    mode           = self.dvar['CONFIG']['mode'].get()

    # Raffraichir l'aide
    txt = self.aide[mode_graphique][mode]
    self.case_aide.Afficher(txt)


  def Voir(self):
    """
       Voir l'etat des variables
    """
    for cle in self.dparam.keys():
       section = self.dparam[cle]['section']
       print section + " - " + cle + " = " + self.dvar[section][cle].get()


  def Construction_Objets(self, init=False):
    """
       Construction des objets Tk
    """

    x  = 10
    x1 = 250
    y  = 10
    dy = 20

    for item in self.liste_section:

       section = item[0]
       frame   = item[1]

       if item[2]: y  = item[2]


       # Affiche le titre de la section
       txt = self.dliste_section[section]
       self.titre[section] = objets.LABEL_TITRE(frame, txt=txt, x=x, y=y)
       y+=dy

       # Produit la liste des parametres de la section
       self.liste_cle[section] = []
       for cle in self.dparam.keys():
          section2 = self.dparam[cle]['section']
          if section2 == section:
             if self.dparam[cle]['type'] == 'liste': self.liste_cle[section].append(cle)
             else:                                   self.liste_cle[section].insert(0, cle)

       # On place 'mode' au debut de la liste
       if section == 'CONFIG':
         self.liste_cle[section].remove('mode')
         self.liste_cle[section].insert(0, 'mode')


       # Affiche chaque parametre
       for cle in self.liste_cle[section]:
          label = self.dparam[cle]['label']
          type_para = self.dparam[cle]['type']
          if   (type_para in ['texte', types.FloatType, 'fichier']):
            self.label[cle] = objets.LABEL(frame, self.dparam, self.dvar, section, cle, x=x, y=y+2, txt=label, fonte=self.fonte, bulle=self.bulle)
            self.objet[cle] = objets.ENTRY(frame, self.dparam, self.dvar, section, cle, x=x1, y=y, largeur=15, fonte=self.fonte, bulle=self.bulle)
            y+=dy
          elif (self.dparam[cle]['type'] == 'liste'):
            y+=0.2*dy
            self.label[cle] = objets.LABEL(frame, self.dparam, self.dvar, section, cle, x=x, y=y+2, txt=label, fonte=self.fonte, bulle=self.bulle)
            self.objet[cle] = objets.LISTE(frame, self.dparam, self.dvar, section, cle, x=x1, y=y, largeur=15, commande=None, fonte=self.fonte, bulle=self.bulle)
            y+=1.4*dy
          else:
            pass
       y+=dy

    y+=dy



  def Scan(self):
    self.Raffraichir_Affichage()
    self.after_id = self.fenetre2.after(50, self.Scan)


  def Quitter(self):

    reponse = tkMessageBox.askokcancel(_(u"Quitter"), _(u"Voulez-vous quitter ? Les paramètres modifiés seront perdus.") )
    if   reponse == 1: reponse = True
    elif reponse == 0: reponse = False
    if not reponse: return

    self.nouveau_para = None
#    self.bulle['state'] = 'none'
#    self.bulle.destroy()
    self.fenetre2.quit()
    self.fenetre2.destroy()


  def Raffraichir_Affichage(self):

    mode_graphique = self.dvar['MODE_GRAPHIQUE']['mode_graphique'].get()
    mode           = self.dvar['CONFIG']['mode'].get()

    # Si les 2 choix mode_graphique et mode n'ont pas changés on n'a pas besoin de raffraichir
    if (self.old_mode_graphique == mode_graphique) and (self.old_mode == mode): return

    # Raffrachir l'aide
    self.Raffraichir_Aide()

    for section in self.dliste_section.keys():
       txt = self.dliste_section[section]
       self.titre[section].Efface()
       self.titre[section].Affiche(txt)

    for section in self.dliste_section.keys():
       for cle in self.liste_cle[section]:
          affiche=True
          # Verification du mode_graphique (Gmsh/Xmgrace, Salome)
          if self.dparam[cle].has_key('mode_graphique'):
            if mode_graphique in self.dparam[cle]['mode_graphique']: affiche=True
            else: affiche=False
          # Verification du mode (LOCAL, DISTANT, WINDOWS)
          if affiche:
             if self.dparam[cle].has_key('mode') and not mode in self.dparam[cle]['mode']: affiche=False

          # Active ou desactive
          if affiche:
             self.label[cle].Active()
             self.objet[cle].Active()
          else:
             self.label[cle].Desactive()
             self.objet[cle].Desactive()

    # Stocke le dernier choix
    self.old_mode_graphique = mode_graphique
    self.old_mode           = mode

    return


  def Renvoi_Parametres(self):

    self.nouveau_para = {}
    for section in self.dliste_section.keys():
       self.nouveau_para[section] = {}
    for cle in self.dparam.keys():
       section = self.dparam[cle]['section']
       self.nouveau_para[section][cle] = self.dvar[section][cle].get()

    self.fenetre2.after_cancel(self.after_id)
    self.fenetre2.quit()
    self.fenetre2.destroy()
    self.fenetre2.quit()


# ==============================================================================

class LABEL_TITRE:

  def __init__(self, frame, txt, x, y, largeur=50, fonte="helvetica 10 bold"):

     self.label = Tk.Label (frame)
     self.label.place(in_=frame,x=x,y=y)
     self.label.configure(borderwidth="1")
     self.label.configure(text=txt)
     self.label.configure(font=fonte)
     self.label.configure(background="wheat")
     self.label.configure(relief="raised")
     self.label.configure(width=largeur)

  def Affiche(self, txt):
     self.label.configure(text=txt)

  def Efface(self):
     self.label.configure(text='')


# ==============================================================================

class LABEL:

  def __init__(self, frame, dparam=None, dvar=None, section=None, cle=None, x=0, y=0, txt='', fonte="helvetica 10 bold", bulle=None):

     self.txt     = txt
     self.dparam  = dparam
     self.dvar    = dvar
     self.section = section
     self.cle     = cle
     self.fonte   = fonte

     self.label = Tk.Label (frame)
     self.label.place(in_=frame,x=x,y=y)
     self.label.configure(borderwidth="1")
     self.Affiche(txt)
     self.label.configure(font=fonte)

     self.enabled = True

     # Bulle d'aide
     if bulle and self.dparam[cle].has_key('bulle'):
       bullemsg = self.dparam[cle]['bulle']
       self.bulle = bulle
       self.bulle.bind_widget(self.label, balloonmsg=bullemsg)

  def Affiche(self, txt):
     self.label.configure(text=txt)

  def Efface(self):
     self.label.configure(text='')

  def Active(self):
     self.Affiche(self.txt)
     self.enabled = True

  def Desactive(self):
     self.Efface()
     self.Affiche('')
     self.enabled = False


# ==============================================================================

class ENTRY:

  def __init__(self, frame, dparam, dvar, section, cle, x, y, largeur=15, fonte="helvetica 14 bold", bulle=None):

     self.dparam  = dparam
     self.dvar    = dvar
     self.section = section
     self.cle     = cle
     self.fonte   = fonte

     self.enabled = True

#      self.valeur = Tk.Entry(frame)
#      self.valeur.place(in_=frame,x=x,y=y)
#      self.valeur.configure(width=largeur)
#      self.valeur.configure(justify="center")
#      self.valeur.configure(textvariable = self.dvar[section][cle])
#      self.valeur.configure(font=fonte)

#     self.valeur = Tk.Entry(frame, textvariable = self.dvar[section][cle], font=fonte, width=largeur, justify="center")
     self.valeur = Tk.Entry(frame)
     self.valeur.place(in_=frame,x=x,y=y)
     self.valeur.configure(width=largeur)
     self.valeur.configure(justify="center")
     self.valeur.configure(textvariable = self.dvar[section][cle])
     self.valeur.configure(font=fonte)


     # Bulle d'aide
     if bulle and self.dparam[cle].has_key('bulle'):
       bullemsg = self.dparam[cle]['bulle']
       self.bulle = bulle
       self.bulle.bind_widget(self.valeur, balloonmsg=bullemsg)


  def Active(self):
    self.valeur.configure(state='normal', background='white')
    self.enabled = True

  def Desactive(self):
    self.valeur.configure(state='disable', background='grey')
    self.enabled = False

  def Change_Valeur(self, valeur):
    self.Enable()
    self.valeur.delete(0,'end')
    self.valeur.insert(0,valeur)
    if not self.enabled: self.Disable()


  def Valide_Valeur(self):
    pass


# ==============================================================================

class LISTE:

  def __init__(self, frame, dparam, dvar, section, cle, x, y, largeur=10, commande=None, fonte="helvetica 10 bold", bulle=None):

     self.dparam   = dparam
     self.dvar     = dvar
     self.section  = section
     self.cle      = cle
     self.fonte    = fonte
     self.commande = commande

     # Tix
     if use_Tix:
        self.liste = Tk.OptionMenu(frame)
        self.liste.place(in_=frame,x=x,y=y)
        self.liste.configure(background="wheat")
        self.liste.configure(highlightbackground="#d9d9d9")
        self.liste.configure(highlightcolor="Black")

        for l in self.dparam[cle]['val_possible']:
           self.liste.add_command(l, label=l, underline=0, font=fonte)

        self.liste.configure(variable=self.dvar[section][cle])
        if commande: self.liste.configure(command=commande)

     else:
        # Tkinter
        variable = self.dvar[section][cle]
        OPTIONS = self.dparam[cle]['val_possible']
        self.liste = apply(Tk.OptionMenu, (frame, variable) + tuple(OPTIONS) )
        self.liste.place(in_=frame,x=x,y=y)
        self.liste.configure(font=self.fonte)


     self.enabled = True


     # Bulle d'aide
     if bulle and self.dparam[cle].has_key('bulle'):
       bullemsg = self.dparam[cle]['bulle']
       self.bulle = bulle
       self.bulle.bind_widget(self.liste, balloonmsg=bullemsg)


  def Change(self, event):
    obj = str(event)
    return True

  def Active(self):
    self.liste.configure(state='normal')
    self.enabled = True

  def Desactive(self):
    self.liste.configure(state='disable')
    self.enabled = False


# ==============================================================================

class BUTTON:

 def __init__(self, frame, x, y, txt, command, width=10, fonte="helvetica 10 bold", background="grey", bulle=None, bullemsg=None):

   self.button = Tk.Button (frame)
   self.button.place(in_=frame,x=x,y=y)
   self.button.configure(command=command)
   self.button.configure(text=txt)
   self.button.configure(font=fonte)
   self.button.configure(width=width)
   self.button.configure(activebackground="black")
   self.button.configure(activeforeground="ivory")
   self.button.configure(background=background)
   self.button.configure(foreground="black")

   # Bulle d'aide
   if bulle and bullemsg:
     self.bulle = bulle
     self.bulle.bind_widget(self.button, balloonmsg=bullemsg)

 def Change_Label(self, txt):
   self.button.configure(text=txt)

 def Active(self):
   self.button.configure(state='normal')
   self.enabled = True

 def Desactive(self):
   self.button.configure(state='disable')
   self.enabled = False

 def destroy(self):
   self.button.destroy()

 def Change_Color(self,color) :
   self.button.configure(background=color)


# ==============================================================================

class CASE_TEXTE:

  def __init__(self, frame, x, y, h, l, params=None, fonte="helvetica 10 bold"):

     self.frame  = frame
     self.params = params
     self.x      = int(x)
     self.y      = int(y)
     self.h      = int(h)
     self.l      = int(l)
     self.fonte  = fonte

     scrollbar = Tk.Scrollbar(frame)
     scrollbar.pack(side=Tk.RIGHT, fill=Tk.Y)

#     self.Case_Texte = Tk.ScrolledText(self.frame)

#     self.Case_Texte = Tk.Text(self.frame)
     self.Case_Texte = Tk.Text(self.frame, wrap=Tk.WORD, yscrollcommand=scrollbar.set)

#     self.Case_Texte.place(in_=self.frame,x=x,y=y)
     self.Case_Texte.pack()

     self.Case_Texte.configure(background="wheat")
     self.Case_Texte.configure(borderwidth="1")
     self.Case_Texte.configure(width=self.l)
     self.Case_Texte.configure(height=self.h)
     self.Case_Texte.configure(highlightbackground="#d9d9d9")
     self.Case_Texte.configure(highlightcolor="Black")
     self.Case_Texte.configure(font=self.fonte)

     self.Case_Texte.insert('end', "this is a ")

     scrollbar.config(command=self.Case_Texte.yview)

#      self.Texte = self.Case_Texte.subwidget_list["text"]
#      self.Texte.configure(background="wheat")
#      self.Texte.configure(font=self.fonte)
#      self.Texte.configure(takefocus="1")
#      self.Texte.configure(wrap="word")  # char, none, or word
#      self.Texte.config(state='disabled')

  def Afficher(self, txt):
#      self.Texte.config(state='normal')
#      self.Texte.delete(1.0, 'end')
#      self.Texte.insert('end', txt )
#      self.Texte.config(state='disabled')

     self.Case_Texte.config(state='normal')
     self.Case_Texte.delete(1.0, 'end')
     self.Case_Texte.insert('end', txt )
     self.Case_Texte.config(state='disabled')




# ==============================================================================

class CANVAS:

  def __init__(self, frame, x, y, h, l, image=None, params=None, anchor='center'):

    self.frame  = frame
    self.params = params
    self.xcanv = x
    self.ycanv = y
    self.hcanv = h
    self.lcanv = l
    self.image = image

    self.canvas = Tk.Canvas(frame)
    self.canvas.place(in_=frame,x=x,y=y, anchor=anchor)
    self.canvas.configure(width=l)
    self.canvas.configure(height=h)
    self.canvas.configure(background='white')
    self.canvas.configure(relief='sunken')

    if image: self.Dessine(image)


# -----------------------------------------------------------------------------------------

  def create_image(self, x1, y1, anchor, image):
    self.canvas.create_image(x1,y1, anchor=anchor, image=image)


# -----------------------------------------------------------------------------------------

  def delete(self, item):
    self.canvas.delete(item)


# -----------------------------------------------------------------------------------------

  def Dessine(self, image):

    # Efface l'ancien graphique
    self.canvas.delete("all")

    if image :
      # Dessine le Canvas
      self.image = Tix.PhotoImage(file = image)
      self.canvas.create_image(0.5*self.lcanv,0.5*self.hcanv,anchor='center',image=self.image)

    # Eventuellement d'autres dessins/tests ici...

    return


# ==============================================================================

class BULLE_PMW:

  def __init__(self, frame):

    self.frame  = frame
    self.bulle = Pmw.Balloon(self.frame)


  def bind_widget(self, objet, balloonmsg):
    self.bulle.bind(objet, balloonmsg)


# ==============================================================================

class BULLE_TIX:

  def __init__(self, frame):

    self.frame  = frame
    self.bulle = Tix.Balloon(self.frame)


  def bind_widget(self, objet, balloonmsg):
    self.bulle.bind(objet, balloonmsg)

# ==============================================================================

