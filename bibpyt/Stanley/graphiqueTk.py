#@ MODIF graphiqueTk Stanley  DATE 06/09/2004   AUTEUR MCOURTOI M.COURTOIS 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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


import Tkinter




class LIGNE_ETAT :


  def __init__(self, frame_parent) :

#    self.var = Tkinter.StringVar()  
    frame_ligne_etat = Tkinter.Frame(frame_parent)
    frame_ligne_etat.pack()
#    self.label = Tkinter.Label(frame_ligne_etat, textvariable=self.var)
    self.label = Tkinter.Label(frame_ligne_etat, text='')
    self.label.pack()
#    self.Affecter("")
  
  
  def Affecter(self, chaine) :
    
#    self.var.set(chaine)
    self.label.configure(text = chaine)
           
# ==============================================================================
    
class MENU :

  """
    MENU DEROULANT SIMPLE
    
  """
  
        
  def __init__ (self, frame_parent, colonnes, items, defaut='', expand = 1) :
  
    """
       IN  frame_parent : objet Tk parent du menu
       IN  colonnes     : liste des titres de colonnes
       IN  item         : dico des items du menu
                           cle = titre des colonnes,
                           resu = [ (item, methode), ...]
    """
       
    self.acces = {}
    
    for col in colonnes :
      titre = Tkinter.Menubutton(frame_parent,text=col,
              relief=Tkinter.FLAT,anchor = Tkinter.NW)
      titre.pack(padx = 3, pady = 1, side = Tkinter.LEFT)
      self.acces[col] = titre
        
      if col not in items.keys() :
        titre['state'] = Tkinter.DISABLED
        
      else :
        choix = Tkinter.Menu(titre,tearoff=0)
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
  
        
  def __init__ (self, frame_parent, titre, liste = None, methode = None, defaut='', expand = 1) :
  
    """
       IN  frame_parent : objet Tk parent du menu
       IN  titre    : titre du menu
       IN  liste    : liste des items du menu (ou None si menu factice)
       IN  methode  : methode invoquee lors d'une selection dans le menu
    """
       
    bouton = Tkinter.Menubutton(frame_parent, text=titre, relief=Tkinter.RAISED)
    bouton.pack(fill=Tkinter.BOTH, expand=expand)
    
    if liste :
      self.methode = methode
      self.select  = Tkinter.StringVar()
      menu = Tkinter.Menu(bouton,tearoff=0)
      for item in liste :
        menu.add_radiobutton(label=item,value=item,variable=self.select,command=self.Activer)
      bouton["menu"] = menu

      if defaut :
        self.select.set(defaut)
      else :
        self.select.set(liste[0])


  def Activer(self) :
  
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
    frame_feu  = Tkinter.Frame(frame_parent)
    frame_feu.pack(side=Tkinter.LEFT,padx=5)

    self.etatfeutot = Tkinter.Canvas(frame_feu, width = 30, height = 100,  
                   background = 'black', border = 0 )
    self.etatfeutot.pack(fill=Tkinter.NONE)

   # dessine l'objet feu tricolore (pour la premiere fois)
    self.etatfeu = [
      self.etatfeutot.create_oval(5, 5, 25, 25,fill='red')    ,
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
  
  def __init__(self, frame_parent, liste, type_selec, defaut = '', fonte = ('Fixed',14,'bold'), hbar = 0) :     


    self.noms    = liste    # liste des noms selectionnables (chaine ou tt objet)
    self.courant = []       # selection courante
    self.indice  = []       # indices de la selection courante
  
   # Barre d'ascenceur
    scrollbar = Tkinter.Scrollbar(frame_parent)
    scrollbar.pack(side=Tkinter.RIGHT, fill=Tkinter.Y) 
    if hbar:
      hscrollbar = Tkinter.Scrollbar(frame_parent)
      hscrollbar.pack(side=Tkinter.BOTTOM, fill=Tkinter.X) 

 
   # Creation de la fenetre de selection 
    if hbar:
      self.listbox = Tkinter.Listbox(
        frame_parent, 
        xscrollcommand  = hscrollbar.set,
        yscrollcommand  = scrollbar.set,
        selectmode      = type_selec,
        exportselection = 0,
        font = fonte,
        )
    else :
      self.listbox = Tkinter.Listbox(
        frame_parent, 
        yscrollcommand  = scrollbar.set,
        selectmode      = type_selec,
        exportselection = 0,
        font = fonte,
        )
      
   # Remplissage
    for chaine in liste :
      self.listbox.insert(Tkinter.END, chaine)
        
   # Affichage
    self.listbox.pack(side=Tkinter.LEFT, expand = 1, fill=Tkinter.BOTH)
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

    
  def Change(self, liste,defaut = '') :
  
    self.listbox.delete(0,Tkinter.END)
    for chaine in liste :
      self.listbox.insert(Tkinter.END,chaine)
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


  def __init__(self, frame, couleur, nom, methode, x=10, y=10) :

    bouton = Tkinter.Button(frame, bg=couleur,text=nom, command=methode)
    bouton.pack(side=Tkinter.LEFT,padx=x,pady=y)  
  
  
# ==============================================================================


class DIALOGUE : 


  def __init__(self, *texte) :
  
    self.rootTk = Tkinter.Tk()
    self.rootTk.wm_title('DIALOGUE ASTER')
    
    frame_haut = Tkinter.Frame(self.rootTk,relief=Tkinter.RAISED,bd=2)
    frame_haut.pack(padx=5,pady=5)
    frame_bas = Tkinter.Frame(self.rootTk)
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


def SAISIE_MODE(l_infos,titre = "") :

  """
    procede a la saisie d'un certain nombre de chaines
    voir classe C_SAISIE
  """
  
  saisie = C_SAISIE_MODE(l_infos,titre)
  return saisie.reponse


class C_SAISIE_MODE :

  """
    Realise la selection du mode lors d'une nouvelle configuration.
    On renvoie juste l'item choisi
  """


  def __init__(self,l_infos, titre) :

    self.root = Tkinter.Tk()
    self.root.title(titre)
    self.l_infos = l_infos

    frame = Tkinter.Frame(self.root)
    frame.grid(padx = 10, pady = 3)

    bouton = Tkinter.Button(self.root, bg='blue',text='OK', command=self.Lire_Mode)
    bouton.grid(row = 1, column=0,pady = 3)  


    self.listbox = Tkinter.Listbox(
      frame, 
      height=3,
      exportselection = 0,
      )

   # Remplissage
    for chaine in l_infos :
      self.listbox.insert(Tkinter.END, chaine)
    self.listbox.selection_set(0)

   # Affichage
    self.listbox.pack(side=Tkinter.LEFT, expand = 0, fill=Tkinter.BOTH)

    self.root.mainloop()
    self.root.destroy()


  def Lire_Mode(self) : 

    items = int(self.listbox.curselection()[0])
    self.reponse = self.l_infos[items]
    self.root.quit()


# ==============================================================================

def SAISIE(l_infos,titre = "", defaut = None) :

  """
    procede a la saisie d'un certain nombre de chaines
    voir classe C_SAISIE
  """
  
  saisie = C_SAISIE(l_infos,titre,defaut)
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


  def __init__(self,l_infos,titre, defaut) :
  
    self.root = Tkinter.Tk()
    self.root.title(titre)
    
    frame = Tkinter.Frame(self.root)
    frame.grid(padx = 10, pady = 10)

    bouton = Tkinter.Button(self.root, bg='blue',text='OK', command=self.Lire)
    bouton.grid(row = 1, column=0,pady = 10)  
            
    row = 0
    self.var = []
    
    for info in l_infos :
      nom = info[0]
      nbr = info[1]
      label = Tkinter.Label(frame,text=nom,padx = 5, pady=2,justify = Tkinter.LEFT)
      label.grid(row = row, column = 0, sticky = Tkinter.W)
      
      rep_ligne = []
      for i in xrange(nbr) :
        var_rep = Tkinter.StringVar(self.root)
        if defaut :
          val_def = defaut[row][i]
          if type(val_def) == type('') :
            var_rep.set(val_def)
          else :
            var_rep.set(repr(val_def))
        rep_ligne.append(var_rep)
        entree = Tkinter.Entry(frame,textvariable = var_rep)
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

# -*- coding: iso-8859-1 -*-




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
    
    frame = Tkinter.Frame(master)
    if x >= y:
      frame.pack(side=Tkinter.TOP)
    else :
      frame.pack(side=Tkinter.LEFT)
    
    self.barre = Tkinter.Canvas(frame, width = x, height = y,  
                  background = 'white',border = 0)
    self.barre.pack(fill=Tkinter.NONE)
    self.rec = None
    self.niveau = 0
    
    
  def Niveau(self,v) :
  
    if v<0 : v=0
    if v>1 : v=1
    
    self.niveau = v
    if self.rec :
      self.barre.delete(self.rec)      
    self.rec = self.barre.create_rectangle(0,0,v*self.x,self.y, fill = 'blue')
    
