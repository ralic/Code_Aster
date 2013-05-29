# coding=utf-8
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
#  OBJETS GRAPHIQUES TK DE HAUT NIVEAU


#import Tix as Tk
import Tkinter as Tk

import types
import graphiqueTk as objets

__fontes__ = 'arial 10 normal'

# ==============================================================================

class LIGNE_ETAT :

  def __init__(self, frame_parent, fonte=__fontes__) :

    frame_ligne_etat = Tk.Frame(frame_parent)
    frame_ligne_etat.pack()
    self.label = Tk.Label(frame_ligne_etat, text='', font=fonte)
    self.label.pack()
#    self.Affecter("")

  def Affecter(self, chaine) :
    self.label.configure(text = chaine)


# ==============================================================================

class MENU :

  """
    MENU DEROULANT SIMPLE

  """

  def __init__ (self, frame_parent, colonnes, items, defaut='', expand = 1, fonte=__fontes__) :

    """
       IN  frame_parent : objet Tk parent du menu
       IN  colonnes     : liste des titres de colonnes
       IN  item         : dico des items du menu
                           cle = titre des colonnes,
                           resu = [ (item, methode), ...]
    """

    self.acces = {}

    for col in colonnes :
      titre = Tk.Menubutton(frame_parent, text=col, relief=Tk.FLAT, anchor = Tk.NW, font=fonte)
      titre.pack(padx = 3, pady = 1, side = Tk.LEFT)
      self.acces[col] = titre

      if col not in items.keys() :
        titre['state'] = Tk.DISABLED

      else :
        choix = Tk.Menu(titre,tearoff=0, font=fonte)
        titre['menu'] = choix

        for item,action in items[col] :
          choix.add_command(label = item, command = action)


# ==============================================================================

class MENU_RADIO_BOX :

  """
    MENU DEROULANT AVEC CHOIX EXCLUSIF

      Attribut prive
        select   : objet Tk permettant la lecture du choix actuel

      Methode privee
        Activer  : Appelle la methode de reaction avec comme parametre la selection actuelle

  """

  def __init__ (self, frame_parent, titre, liste = None, methode = None, defaut='', expand = 1, fonte=__fontes__):

    """
       IN  frame_parent : objet Tk parent du menu
       IN  titre    : titre du menu
       IN  liste    : liste des items du menu (ou None si menu factice)
       IN  methode  : methode invoquee lors d'une selection dans le menu
    """

    bouton = Tk.Menubutton(frame_parent, text=titre, relief=Tk.RAISED, font=fonte)
    bouton.pack(fill=Tk.BOTH, expand=expand)

    if liste :
      self.methode = methode
      self.select  = Tk.StringVar()
      menu = Tk.Menu(bouton,tearoff=0, font=fonte)
      for item in liste :
        menu.add_radiobutton(label=item,value=item,variable=self.select,command=self.Activer)
      bouton["menu"] = menu

      if defaut :
        self.select.set(defaut)
      else :
        self.select.set(liste[0])


  def Activer(self):
    self.methode(self.select.get())


# ==============================================================================

class FEU_TRICOLORE :

  """
    Feu tricolore d'etat :
       vert   : OK pas de calcul,
       orange : OK avec calcul,
       rouge  : NOOK
  """

  liste_couleurs = {
    'red'    : 0,
    'orange' : 1,
    'green'  : 2
    }

  def __init__(self, frame_parent, defaut ='red') :

    self.couleur = defaut   # couleur actuelle (par defaut) du feu

    # cree l'objet feu tricolore
    frame_feu  = Tk.Frame(frame_parent)
    frame_feu.pack(side=Tk.LEFT,padx=5)

    self.etatfeutot = Tk.Canvas(frame_feu, width = 30, height = 100, background = 'black', border = 0)
    self.etatfeutot.pack(fill=Tk.NONE)

    # dessine l'objet feu tricolore (pour la premiere fois)
    self.etatfeu = [ self.etatfeutot.create_oval(5, 5, 25, 25,fill='red')    ,
                     self.etatfeutot.create_oval(5, 30, 25, 50,fill='white') ,
                     self.etatfeutot.create_oval(5, 55, 25, 75, fill='white')
                   ]


  def Changer_couleur(self,nouvelle_couleur) :

    '''On change la couleur du feu tricolore et on le redessine'''

    if nouvelle_couleur <> self.couleur :

      # On met a blanc l'ancienne couleur
      position = FEU_TRICOLORE.liste_couleurs[self.couleur]
      self.etatfeutot.itemconfigure(self.etatfeu[position],fill='white')

      # On affecte la nouvelle couleur
      self.couleur = nouvelle_couleur

      # On colorie la nouvelle couleur
      position = FEU_TRICOLORE.liste_couleurs[self.couleur]
      self.etatfeutot.itemconfigure(self.etatfeu[position],fill=self.couleur)


# ==============================================================================

class LIST_BOX :

  """
    FENETRE DE SELECTION

     Attributs publics
      courant    : liste des noms selectionnes
      noms       : liste des noms selectionnables
      indice     : liste des indices des noms selectionnes par rapport a noms

     Methode publique
      Scan       : Rafraichit la selection, retourne vrai si elle a change
      Change     : Affecte une nouvelle liste de noms selectionnables

     Attributs prives
      listbox    : objet Tk de type listbox

  """

  def __init__(self, frame_parent, liste, type_selec, defaut = '', hbar = 0, fonte = __fontes__):


    self.noms    = liste    # liste des noms selectionnables (chaine ou tt objet)
    self.courant = []       # selection courante
    self.indice  = []       # indices de la selection courante

    # Barre d'ascenceur
    scrollbar = Tk.Scrollbar(frame_parent)
    scrollbar.pack(side=Tk.RIGHT, fill=Tk.Y)
    if hbar:
      hscrollbar = Tk.Scrollbar(frame_parent)
      hscrollbar.pack(side=Tk.BOTTOM, fill=Tk.X)


    # Creation de la fenetre de selection
    if hbar:
      self.listbox = Tk.Listbox( frame_parent,
                                 xscrollcommand  = hscrollbar.set,
                                 yscrollcommand  = scrollbar.set,
                                 selectmode      = type_selec,
                                 exportselection = 0,
                                 font = fonte,
                                    )
    else :
      self.listbox = Tk.Listbox( frame_parent,
                                 yscrollcommand  = scrollbar.set,
                                 selectmode      = type_selec,
                                 exportselection = 0,
                                 font = fonte,
                                    )

    # Remplissage
    for chaine in liste :
      self.listbox.insert(Tk.END, chaine)

    # Affichage
    self.listbox.pack(side=Tk.LEFT, expand = 1, fill=Tk.BOTH)
    scrollbar.config(command=self.listbox.yview)
    if hbar : hscrollbar.config(command=self.listbox.xview)

    # Pre-selection du defaut
    try :
      p = liste.index(defaut)
      self.listbox.selection_set(p)
      self.courant = [defaut]
      self.indice  = [p]
    except ValueError :
      pass


  def Selectionne(self, valeur) :

      p = self.noms.index(valeur)
      self.listbox.selection_clear(0,len(self.noms))
      self.listbox.selection_set(p,p)
      self.courant = [valeur]
      self.indice  = [p]


  def Change(self, liste, defaut = '') :

    self.listbox.delete(0,Tk.END)
    for chaine in liste :
      self.listbox.insert(Tk.END,chaine)
    self.noms = liste
    self.courant = []

   # Pre-selection du defaut
    try :
      p = liste.index(defaut)
      self.listbox.selection_set(p)
      self.courant = [defaut]
      self.indice  = [p]
    except ValueError :
      pass


  def Scan(self) :

    positions = map(int, self.listbox.curselection())
    positions.sort()    # on garde la selection du haut vers le bas

    nouveau = []
    for pos in positions :
      nouveau.append(self.noms[pos])
    if nouveau <> self.courant :
      different = 1
      self.courant = nouveau
      self.indice  = positions

    else :
      different = 0

    return different


# ==============================================================================

class BOUTON :

  def __init__(self, frame, couleur, nom, methode, x=10, y=10, fonte=__fontes__) :

    bouton = Tk.Button(frame, bg=couleur,text=nom, command=methode, font=fonte)
    bouton.pack(side=Tk.LEFT,padx=x,pady=y)


# ==============================================================================

class DIALOGUE :

  def __init__(self, *texte) :

    self.rootTk = Tk.Tk()
    self.rootTk.wm_title('DIALOGUE ASTER')

    frame_haut = Tk.Frame(self.rootTk,relief=Tk.RAISED,bd=2)
    frame_haut.pack(padx=5,pady=5)
    frame_bas = Tk.Frame(self.rootTk)
    frame_bas.pack(pady=0)

    ch = ' '*50 + '\n'
    for ligne in texte :
      ch = ch + ligne + '\n'

    le = LIGNE_ETAT(frame_haut)
    le.Affecter(ch)

    BOUTON(frame_bas,'IndianRed1',"OK",self.rootTk.destroy)

    self.Action_evenement()
    self.rootTk.mainloop()

  def Action_evenement(self) :
    self.rootTk.after(30, self.Action_evenement)


# ==============================================================================

def SAISIE_MODE(l_infos, titre = "", fonte=__fontes__, type_selec=Tk.SINGLE, vbar = 1) :

  """
    procede a la saisie d'un certain nombre de chaines
    voir classe C_SAISIE
  """

  saisie = C_SAISIE_MODE(l_infos, titre, fonte, type_selec, vbar)
  return saisie.reponse


class C_SAISIE_MODE :

  """
    Realise la selection du mode lors d'une nouvelle configuration.
    On renvoie juste l'item choisi
  """


  def __init__(self,l_infos, titre, fonte=__fontes__, type_selec=Tk.SINGLE, vbar = 1) :

    self.root = Tk.Tk()
    self.root.title(titre)
    self.l_infos = l_infos
    self.type_selec = type_selec

    frame = Tk.Frame(self.root)
    frame.grid(padx = 20, pady = 3)

    bouton = Tk.Button(self.root, bg='blue',text='OK', command=self.Lire_Mode)
    bouton.grid(row= 1, column=0, pady = 3)

    # Barre d'ascenceur
    if vbar:
      scrollbar = Tk.Scrollbar(frame)
      scrollbar.pack(side=Tk.RIGHT, fill=Tk.Y)

    # Creation de la fenetre de selection
    if vbar:
      self.listbox = Tk.Listbox( frame,
                                 height=3,
                                 exportselection = 0,
                                 selectmode      = type_selec,
                                 yscrollcommand  = scrollbar.set,
                                 font = fonte,
                                    )
    else :
      self.listbox = Tk.Listbox( frame,
                                 height=3,
                                 exportselection = 0,
                                 selectmode      = type_selec,
                                 font = fonte,
                              )


    # Remplissage
    for chaine in l_infos :
      self.listbox.insert(Tk.END, chaine)
    self.listbox.selection_set(0)

    # Affichage
    self.listbox.pack(side=Tk.LEFT, expand = Tk.YES, fill=Tk.BOTH)
    if vbar: scrollbar.config(command=self.listbox.yview)

    self.root.mainloop()
    self.root.destroy()


  def Lire_Mode(self) :

    items = int(self.listbox.curselection()[0])
    self.reponse = self.l_infos[items]

    if self.type_selec == Tk.EXTENDED:
      items = map(int, self.listbox.curselection())
      self.reponse = map( lambda x: self.l_infos[x], items )

    self.root.quit()


# ==============================================================================

def SAISIE(l_infos,titre = "", defaut = None, fonte=__fontes__) :

  """
    procede a la saisie d'un certain nombre de chaines
    voir classe C_SAISIE
  """

  saisie = C_SAISIE(l_infos,titre,defaut,fonte)
  return saisie.reponse


class C_SAISIE :

  """
    Realise la saisie de chaines de caractere

    La presentation s'appuie sur des lignes titrees avec plusieurs champs
    de reponse par ligne.

    Les informations de presentation sont donnees par une double liste :
    [ [nom_ligne_1,nbr de champs ligne 1],...]

    La reponse est elle aussi envoyee sous forme d'une double liste :
    [[champ_1 ligne_1, ..., champ_n ligne_1], ...]
  """


  def __init__(self,l_infos,titre, defaut, fonte=__fontes__) :

    self.root = Tk.Tk()
    self.root.title(titre)

    frame = Tk.Frame(self.root)
    frame.grid(padx = 10, pady = 10)

    bouton = Tk.Button(self.root, bg='blue',text='OK', command=self.Lire)
    bouton.grid(row = 1, column=0,pady = 10)

    row = 0
    self.var = []

    for info in l_infos :
      nom = info[0]
      nbr = info[1]
      label = Tk.Label(frame, text=nom, padx = 5, pady=2, justify = Tk.LEFT, font=fonte)
      label.grid(row = row, column = 0, sticky = Tk.W)

      rep_ligne = []
      for i in xrange(nbr) :
        var_rep = Tk.StringVar(self.root)
        if defaut :
          val_def = defaut[row][i]
          if type(val_def) == type('') :
            var_rep.set(val_def)
          else :
            var_rep.set(repr(val_def))
        rep_ligne.append(var_rep)
        entree = Tk.Entry(frame,textvariable = var_rep, font=fonte)
        entree.grid(row=row,column=i+1,padx=2)

      self.var.append(rep_ligne)
      row = row + 1

    self.root.mainloop()
    self.root.destroy()



  def Lire(self) :

    self.reponse = []
    for ligne in self.var :
      rep_ligne = []
      for var in ligne :
        item = var.get()
        rep_ligne.append(item)
      self.reponse.append(rep_ligne)
    self.root.quit()


# ==============================================================================

class CASE_A_COCHER :

  """
    Case a cocher de base
  """

  def __init__(self, frame, x, y, txt, fonte=__fontes__) :

    self.var = Tk.IntVar()
    self.var.set(0)

    self.check = Tk.Checkbutton(frame, variable=self.var, command=self.Check_Commande)
    self.check.pack(side=Tk.LEFT,padx=x,pady=y)
    self.check.configure(text=txt, font=fonte)

  def Check_Commande(self):
    # Permet d'effectuer une action lors de la selection/deselection
    return True

  def Valeur(self):
    return self.var.get()


# ==============================================================================

class BARRE :

  """
    Barre de niveau (largeur x, hauteur y)

    Attribut :
      niveau : dernier niveau fixe (0 a l'initialisation)

    Methodes :
      Niveau : fixe le niveau de la barre (compris entre 0 et 1)
  """

  def __init__(self, master,x,y) :

    self.x = x
    self.y = y

    frame = Tk.Frame(master)
    if x >= y:
      frame.pack(side=Tk.TOP)
    else :
      frame.pack(side=Tk.LEFT)

    self.barre = Tk.Canvas(frame, width = x, height = y,
                  background = 'white',border = 0)
    self.barre.pack(fill=Tk.NONE)
    self.rec = None
    self.niveau = 0


  def Niveau(self,v) :

    if v<0 : v=0
    if v>1 : v=1

    self.niveau = v
    if self.rec :
      self.barre.delete(self.rec)
    self.rec = self.barre.create_rectangle(0,0,v*self.x,self.y, fill = 'blue')


# ==============================================================================
