#@ MODIF Graph Utilitai  DATE 06/09/2004   AUTEUR MCOURTOI M.COURTOIS 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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

# -*- coding: iso-8859-1 -*-

# RESPONSABLE MCOURTOI M.COURTOIS

import aster
import os.path
import string
import types

#-------------------------------------------------
class Graph:
   """
   Cette classe définit l'objet Graph pour Code_Aster.
   
   Important :  Utiliser les méthodes dédiées à la manipulation des données
      (AjoutCourbe, ...) car elles tiennent à jour les attributs "privés"
      relatifs aux données : NbCourbe, les extrema...
   
   Attributs :
   - Données de chaque courbe :
      .Valeurs   : liste des valeurs de chaque courbe, pour chaque courbe :
         (paramètres, parties réelles [, parties imaginaires])
      .Legendes  : liste des noms de chaque courbe
      .Labels    : liste des noms des colonnes de chaque courbe
      .Styles    : liste des infices de styles de ligne
      .Couleurs  : liste des indices de couleurs
      .Marqueurs : liste des indices de symboles/marqueurs
      .FreqMarq  : liste des fréquences des marqueurs
     Pour Lignes, Couleurs, Marqueurs, FreqMarq, -1 signifie valeur par défaut
     du traceur.

   - Propriétés :
      .Titre : titre du graphique
      .SousTitre : sous-titre (appelé commentaire dans agraf)
   - Axes :
      .Min_X, .Max_X, .Min_Y, .Max_Y : bornes du tracé (une méthode permet de
         renseigner automatiquement ces valeurs avec les extréma globaux)
      .Legende_X, .Legende_Y : légende des axes
      .Echelle_X, .Echelle_Y : type d'échelle (LIN, LOG)
      .Grille_X, .Grille_Y : paramètre de la grille (pas ou fréquence au choix
         de l'utilisateur en fonction du traceur qu'il veut utiliser)
      .Tri      : tri à effectuer sur les données

   Attributs privés (modifiés uniquement par les méthodes de la classe) :
      .NbCourbe : nombre de courbes
      .BBXmin, BBXmax, BBYmin, BBYmax : extrema globaux (bounding box)
   """
#-------------------------------------------------
   def __init__(self):
      """
      Construction + valeurs par défaut des attributs
      """
      self.Valeurs   = []
      self.Legendes  = []
      self.Labels    = []
      self.Styles    = []
      self.Couleurs  = []
      self.Marqueurs = []
      self.FreqMarq  = []
      self.Titre     = ''
      self.SousTitre = ''
      self.Min_X     =  1.e+99
      self.Max_X     = -1.e+99
      self.Min_Y     =  1.e+99
      self.Max_Y     = -1.e+99
      self.Legende_X = ''
      self.Legende_Y = ''
      self.Echelle_X = 'LIN'
      self.Echelle_Y = 'LIN'
      self.Grille_X  = -1
      self.Grille_Y  = -1
      self.Tri       = ''
      # attributs que l'utilisateur ne doit pas modifier
      self.NbCourbe  = len(self.Valeurs)
      self.BBXmin    = self.Min_X
      self.BBXmax    = self.Max_X
      self.BBYmin    = self.Min_Y
      self.BBYmax    = self.Max_Y
      return
#-------------------------------------------------
   def SetExtrema(self):
      """
      Remplit les limites du tracé (Min/Max_X/Y) avec les valeurs de la
      bounding box
      """
      self.Min_X = self.BBXmin
      self.Max_X = self.BBXmax
      self.Min_Y = self.BBYmin
      self.Max_Y = self.BBYmax
      return
#-------------------------------------------------
   def AutoBB(self,debut=-1):
      """
      Met à jour automatiquement la "bounding box"
      (extrema toutes courbes confondues)
      Appelé par les méthodes de manipulation des données
      """
      if debut == -1:
         debut=self.NbCourbe-1
      if debut == 0:
         X0 =  1.e+99
         X1 = -1.e+99
         Y0 =  1.e+99
         Y1 = -1.e+99
      else:
         X0 = self.BBXmin
         X1 = self.BBXmax
         Y0 = self.BBYmin
         Y1 = self.BBYmax
      
      for i in range(debut,self.NbCourbe):
         X0 = min([X0,]+list(self.Valeurs[i][0]))
         X1 = max([X1,]+list(self.Valeurs[i][0]))
         for ny in range(1,len(self.Valeurs[i])):
            Y0 = min([Y0,]+list(self.Valeurs[i][ny]))
            Y1 = max([Y1,]+list(self.Valeurs[i][ny]))
      self.BBXmin = X0
      self.BBXmax = X1
      self.BBYmin = Y0
      self.BBYmax = Y1
      return
#-------------------------------------------------
   def AjoutCourbe(self,Val,Lab,Leg='',Sty=-1,Coul=-1,Marq=-1,FreqM=-1):
      """
      Ajoute une courbe dans les données
         Val   : liste de 2 listes (ou 3 si complexe) : abs, ord[, imag]
         Leg   : une chaine
         Lab   : liste de 2 chaines (ou 3 si complexe)
         Sty   : un entier
         Coul  : un entier
         Marq  : un entier
         FreqM : un entier
      Met à jour les attributs : NbCourbe, BBXmin/Xmax/Ymin/Ymax
      """
      nbc = len(Val)   # nombre de colonnes : 2 ou 3
      
      # verifications : "if not (conditions requises)"
      if not ( 2 <= nbc <= 3 and \
         type(Val[0]) in (types.ListType, types.TupleType) and \
         type(Val[1]) in (types.ListType, types.TupleType) and \
         (nbc==2 or type(Val[2]) in (types.ListType, types.TupleType)) and \
         len(Val[0]) == len(Val[1]) and (nbc==2 or len(Val[0]) == len(Val[2])) ):
            self.Error('<Graph.AjoutCourbe> "Val" doit etre une liste de 2 ou 3 listes de rééls de meme longueur')
      
      if len(Lab) != nbc:
            self.Error('<Graph.AjoutCourbe> "Lab" doit etre une liste de 2 ou 3 chaines')
            
      # ajout dans les données
      self.Legendes.append(str(Leg))
      self.Labels.append([str(L) for L in Lab])
      self.Valeurs.append(Val)
      self.Styles.append(Sty)
      self.Couleurs.append(Coul)
      self.Marqueurs.append(Marq)
      self.FreqMarq.append(FreqM)

      self.NbCourbe = self.NbCourbe + 1
      self.AutoBB()
      return
#-------------------------------------------------
   def Courbe(self,n):
      """
      Permet de récupérer les données de la courbe d'indice n sous forme
      d'un dictionnaire.
      """
      dico={
         'Leg'    : self.Legendes[n],           # légende de la courbe
         'LabAbs' : self.Labels[n][0],          # labels des abscisses
         'LabOrd' : [self.Labels[n][1],],       # labels des ordonnées
         'NbCol'  : len(self.Valeurs[n]),       # nombre de colonnes
         'NbPts'  : len(self.Valeurs[n][0]),    # nombre de points
         'Abs'    : self.Valeurs[n][0],         # liste des abscisses
         'Ord'    : [self.Valeurs[n][1],],      # liste des ordonnées
         'Sty'    : self.Styles[n],             # style de la ligne
         'Coul'   : self.Couleurs[n],           # couleur
         'Marq'   : self.Marqueurs[n],          # marqueur
         'FreqM'  : self.FreqMarq[n]            # fréquence du marqueur
      }
      if(dico['NbCol'] == 3):
         dico['LabOrd'].append(self.Labels[n][2]) # labels de la partie imaginaire
         dico['Ord'].append(self.Valeurs[n][2])   # liste des ordonnées partie imaginaire
      return dico
#-------------------------------------------------
   def Error(self,msg):
      """
      Gère l'affichage d'un message d'erreur et lève une exception aster
      """
      raise aster.error, ' <S> '+msg
      return
#-------------------------------------------------
   def __repr__(self):
      """
      Affichage du contenu d'un Graph
      """
      srep=''
      for attr in ['NbCourbe','Legendes','Labels','Valeurs','Min_X','Max_X','Min_Y','Max_Y','BBXmax','BBXmin','BBYmax','BBYmin','Legende_X','Legende_Y','Echelle_X','Echelle_Y','Grille_X','Grille_Y','Tri']:
         srep=srep + '%-10s : %s\n' % (attr,str(getattr(self,attr)))
      return srep


#-------------------------------------------------
#-------------------------------------------------
#-------------------------------------------------
class ImprGraph:
   """
   Cette classe définit l'impression d'un objet Graph dans un fichier.
   
   Attributs :
      .NomFich : liste de noms de fichier de sortie

   Attributs privés (modifiés uniquement par les méthodes de la classe) :
      .Fich    : liste des objets 'fichier'
      .Graph   : objet Graph que l'on veut tracer
      .DicForm : dictionnaire des formats de base (séparateur, format des réels...)
   
   Les méthodes Entete, DescrCourbe, Trace (définition de l'entete, partie descriptive
   d'une courbe, méthode de tracé/impressiion) sont définies dans une classe dérivée.
   """
#-------------------------------------------------
   def __init__(self,graph,nomfich,dform=None):
      """
      Construction, ouverture du fichier, surcharge éventuelle du formatage
      """
      # Ouverture du(des) fichier(s)
      self.NomFich=[]
      if type(nomfich) is types.StringType:
         self.NomFich.append(nomfich)
      elif type(nomfich) in (types.ListType, types.TupleType):
         self.NomFich=nomfich
      self.Fich=[]
      for ff in self.NomFich:
         self.Fich.append(open(ff,'w'))
      
      # objet Graph
      self.Graph=graph
      
      # formats de base
      self.DicForm={
         'csep'  : ' ',       # séparateur
         'ccom'  : '#',       # commentaire
         'cdeb'  : '',        # début de ligne
         'cfin'  : '\n',      # fin de ligne
         'formK' : '%-12s',   # chaines
         'formR' : '%12.5E',  # réels
         'formI' : '%12d'     # entiers
      }
      if dform!=None and type(dform)==types.DictType:
         for k,v in dform.items():  self.DicForm['k']=v
      return
#-------------------------------------------------
   def __del__(self):
      """
      Fermeture du(des) fichier(s) à la destruction
      """
      for fp in self.Fich:
         fp.close()
      return
#-------------------------------------------------
   def Entete(self):
      """
      Retourne l'entete
      """
      raise StandardError, "Cette méthode doit etre définie par la classe fille."
#-------------------------------------------------
   def DescrCourbe(self,**args):
      """
      Retourne la chaine de caractères décrivant les paramètres de la courbe.
      """
      raise StandardError, "Cette méthode doit etre définie par la classe fille."
#-------------------------------------------------
   def Trace(self):
      """
      Méthode pour 'tracer' l'objet Graph dans un fichier.
      Met en page l'entete, la description des courbes et les valeurs selon
      le format et ferme le fichier.
      """
      raise StandardError, "Cette méthode doit etre définie par la classe fille."


#-------------------------------------------------
#-------------------------------------------------
#-------------------------------------------------
class ImprTableau(ImprGraph):
   """
   Impression d'un objet Graph sous forme d'un tableau de colonnes,
   on suppose que les courbes partagent la meme liste d'abscisse.
   """
#-------------------------------------------------
   def Entete(self):
      """
      Entete du tableau
      """
      entete=[]
      g=self.Graph
      # titre / sous-titre
      entete.append(self.DicForm['ccom']+' '+g.Titre+self.DicForm['cfin'])
      entete.append(self.DicForm['ccom']+' '+g.SousTitre+self.DicForm['cfin'])
      # legendes
      for i in range(g.NbCourbe):
         entete.append(self.DicForm['ccom']+' Courbe '+str(i)+' '+g.Legendes[i]+self.DicForm['cfin'])
      # labels de colonne
      entete.append(self.DicForm['formK'] % g.Labels[0][0])
      for i in range(g.NbCourbe):
         for lab in g.Labels[i][1:]:
            entete.append(self.DicForm['csep']+(self.DicForm['formK'] % lab))
      entete.append(self.DicForm['cfin'])
      return entete
#-------------------------------------------------
   def DescrCourbe(self,**args):
      """
      Retourne la chaine de caractères décrivant les paramètres de la courbe.
      Sans objet pour un tableau.
      """
      pass
#-------------------------------------------------
   def Trace(self):
      """
      Méthode pour 'tracer' l'objet Graph dans un fichier.
      Met en page l'entete, la description des courbes et les valeurs selon
      le format et ferme le fichier.
      """
      fich=self.Fich[0]
      g=self.Graph
      if g.NbCourbe < 1:
         fich.close()
         return
      # entete
      for lig in self.Entete():
         fich.write(lig)
      # valeurs
      dC0=g.Courbe(0)
      for j in range(dC0['NbPts']):
         sv=self.DicForm['formR'] % dC0['Abs'][j]
         fich.write(sv)
         for i in range(g.NbCourbe):
            dCi=g.Courbe(i)
            for k in range(dCi['NbCol']-1):
               sv=self.DicForm['formR'] % dCi['Ord'][k][j]
               fich.write(self.DicForm['csep']+sv)
         fich.write(self.DicForm['cfin'])
      fich.close()
      return


#-------------------------------------------------
#-------------------------------------------------
#-------------------------------------------------
class ImprXmgrace(ImprGraph):
   """
   Impression d'un objet Graph au format XMGRACE.
   Attribut supplémentaire : .Pilote
   """
   Pilote=''
#-------------------------------------------------
   def Entete(self):
      """
      Retourne l'entete du fichier .agr correspondant à la mise en forme
      """
      dic_ech={ 'LIN' : 'Normal', 'LOG' : 'Logarithmic' }
      g=self.Graph
      entete=[]
      entete.append("""
# Grace project file
#
@version 50100
@page size 842, 595
@page scroll 5%
@page inout 5%
@link page off
@map font 0 to "Times-Roman", "Times-Roman"
@map font 1 to "Times-Italic", "Times-Italic"
@map font 2 to "Times-Bold", "Times-Bold"
@map font 3 to "Times-BoldItalic", "Times-BoldItalic"
@map font 4 to "Helvetica", "Helvetica"
@map font 5 to "Helvetica-Oblique", "Helvetica-Oblique"
@map font 6 to "Helvetica-Bold", "Helvetica-Bold"
@map font 7 to "Helvetica-BoldOblique", "Helvetica-BoldOblique"
@map font 8 to "Courier", "Courier"
@map font 9 to "Courier-Oblique", "Courier-Oblique"
@map font 10 to "Courier-Bold", "Courier-Bold"
@map font 11 to "Courier-BoldOblique", "Courier-BoldOblique"
@map font 12 to "Symbol", "Symbol"
@map font 13 to "ZapfDingbats", "ZapfDingbats"
@map color 0 to (255, 255, 255), "white"
@map color 1 to (0, 0, 0), "black"
@map color 2 to (255, 0, 0), "red"
@map color 3 to (0, 255, 0), "green"
@map color 4 to (0, 0, 255), "blue"
@map color 5 to (255, 255, 0), "yellow"
@map color 6 to (188, 143, 143), "brown"
@map color 7 to (220, 220, 220), "grey"
@map color 8 to (148, 0, 211), "violet"
@map color 9 to (0, 255, 255), "cyan"
@map color 10 to (255, 0, 255), "magenta"
@map color 11 to (255, 165, 0), "orange"
@map color 12 to (114, 33, 188), "indigo"
@map color 13 to (103, 7, 72), "maroon"
@map color 14 to (64, 224, 208), "turquoise"
@map color 15 to (0, 139, 0), "green4"
@reference date 0
@date wrap off
@date wrap year 1950
@timestamp off
@default linewidth 1.0
@default linestyle 1
@default color 1
@default pattern 1
@default font 0
@default char size 1.000000
@default symbol size 1.000000
@default sformat "%.8g"
@background color 0
@page background fill on
@r0 off
@link r0 to g0
@r0 type above
@r0 linestyle 1
@r0 linewidth 1.0
@r0 color 1
@r0 line 0, 0, 0, 0
@r1 off
@link r1 to g0
@r1 type above
@r1 linestyle 1
@r1 linewidth 1.0
@r1 color 1
@r1 line 0, 0, 0, 0
@r2 off
@link r2 to g0
@r2 type above
@r2 linestyle 1
@r2 linewidth 1.0
@r2 color 1
@r2 line 0, 0, 0, 0
@r3 off
@link r3 to g0
@r3 type above
@r3 linestyle 1
@r3 linewidth 1.0
@r3 color 1
@r3 line 0, 0, 0, 0
@r4 off
@link r4 to g0
@r4 type above
@r4 linestyle 1
@r4 linewidth 1.0
@r4 color 1
@r4 line 0, 0, 0, 0
@g0 on
@g0 hidden false
@g0 type XY
@g0 stacked false
@g0 bar hgap 0.000000
@with g0
@    stack world 0, 0, 0, 0
@    znorm 1
@    view xmin 0.150000
@    view xmax 1.150000
@    view ymin 0.150000
@    view ymax 0.850000
@    title font 0
@    title size 1.500000
@    title color 1
@    subtitle font 0
@    subtitle size 1.000000
@    subtitle color 1
@    xaxes invert off
@    yaxes invert off
@    xaxis  on
@    xaxis  type zero false
@    xaxis  offset 0.000000 , 0.000000
@    xaxis  bar on
@    xaxis  bar color 1
@    xaxis  bar linestyle 1
@    xaxis  bar linewidth 1.0
@    xaxis  label layout para
@    xaxis  label place auto
@    xaxis  label char size 1.000000
@    xaxis  label font 0
@    xaxis  label color 1
@    xaxis  label place normal
@    xaxis  tick on
@    xaxis  tick minor ticks 1
@    xaxis  tick default 6
@    xaxis  tick place rounded true
@    xaxis  tick in
@    xaxis  tick major size 1.000000
@    xaxis  tick major color 1
@    xaxis  tick major linewidth 1.0
@    xaxis  tick major linestyle 2
@    xaxis  tick major grid on
@    xaxis  tick minor color 1
@    xaxis  tick minor linewidth 1.0
@    xaxis  tick minor linestyle 1
@    xaxis  tick minor grid off
@    xaxis  tick minor size 0.500000
@    xaxis  ticklabel on
@    xaxis  ticklabel format general
@    xaxis  ticklabel prec 5
@    xaxis  ticklabel angle 0
@    xaxis  ticklabel skip 0
@    xaxis  ticklabel stagger 0
@    xaxis  ticklabel place normal
@    xaxis  ticklabel offset auto
@    xaxis  ticklabel offset 0.000000 , 0.010000
@    xaxis  ticklabel start type auto
@    xaxis  ticklabel start 0.000000
@    xaxis  ticklabel stop type auto
@    xaxis  ticklabel stop 0.000000
@    xaxis  ticklabel char size 1.000000
@    xaxis  ticklabel font 0
@    xaxis  ticklabel color 1
@    xaxis  ticklabel formula ""
@    xaxis  ticklabel append ""
@    xaxis  ticklabel prepend ""
@    xaxis  tick place both
@    xaxis  tick spec type none
@    yaxis  on
@    yaxis  type zero false
@    yaxis  offset 0.000000 , 0.000000
@    yaxis  bar on
@    yaxis  bar color 1
@    yaxis  bar linestyle 1
@    yaxis  bar linewidth 1.0
@    yaxis  label layout para
@    yaxis  label place auto
@    yaxis  label char size 1.000000
@    yaxis  label font 0
@    yaxis  label color 1
@    yaxis  label place normal
@    yaxis  tick on
@    yaxis  tick minor ticks 1
@    yaxis  tick default 6
@    yaxis  tick place rounded true
@    yaxis  tick in
@    yaxis  tick major size 1.000000
@    yaxis  tick major color 1
@    yaxis  tick major linewidth 1.0
@    yaxis  tick major linestyle 2
@    yaxis  tick major grid on
@    yaxis  tick minor color 1
@    yaxis  tick minor linewidth 1.0
@    yaxis  tick minor linestyle 1
@    yaxis  tick minor grid off
@    yaxis  tick minor size 0.500000
@    yaxis  ticklabel on
@    yaxis  ticklabel format general
@    yaxis  ticklabel prec 5
@    yaxis  ticklabel angle 0
@    yaxis  ticklabel skip 0
@    yaxis  ticklabel stagger 0
@    yaxis  ticklabel place normal
@    yaxis  ticklabel offset auto
@    yaxis  ticklabel offset 0.000000 , 0.010000
@    yaxis  ticklabel start type auto
@    yaxis  ticklabel start 0.000000
@    yaxis  ticklabel stop type auto
@    yaxis  ticklabel stop 0.000000
@    yaxis  ticklabel char size 1.000000
@    yaxis  ticklabel font 0
@    yaxis  ticklabel color 1
@    yaxis  ticklabel formula ""
@    yaxis  ticklabel append ""
@    yaxis  ticklabel prepend ""
@    yaxis  tick place both
@    yaxis  tick spec type none
@    altxaxis  off
@    altyaxis  off
@    legend on
@    legend loctype view
@    legend 0.85, 0.8
@    legend box color 1
@    legend box pattern 1
@    legend box linewidth 1.0
@    legend box linestyle 1
@    legend box fill color 0
@    legend box fill pattern 1
@    legend font 0
@    legend char size 1.000000
@    legend color 1
@    legend length 4
@    legend vgap 1
@    legend hgap 1
@    legend invert false
@    frame type 0
@    frame linestyle 1
@    frame linewidth 1.0
@    frame color 1
@    frame pattern 1
@    frame background color 0
@    frame background pattern 0
""")
      entete.append('@    title "'+g.Titre+'"\n')
      entete.append('@    subtitle "'+g.SousTitre+'"\n')
      entete.append('@    xaxis  label "'+g.Legende_X+'"\n')
      entete.append('@    yaxis  label "'+g.Legende_Y+'"\n')
      entete.append('@    xaxes scale '+dic_ech[g.Echelle_X]+'\n')
      entete.append('@    yaxes scale '+dic_ech[g.Echelle_Y]+'\n')
      entete.append('@    xaxis  tick major '+str(g.Grille_X)+'\n')
      entete.append('@    yaxis  tick major '+str(g.Grille_Y)+'\n')
      entete.append('@    world xmin '+str(g.Min_X)+'\n')
      entete.append('@    world xmax '+str(g.Max_X)+'\n')
      entete.append('@    world ymin '+str(g.Min_Y)+'\n')
      entete.append('@    world ymax '+str(g.Max_Y)+'\n')
      return entete
#-------------------------------------------------
   def DescrCourbe(self,**args):
      """
      Retourne la chaine de caractères décrivant les paramètres de la courbe.
      """
      # valeurs par défaut
      sty=args['Sty']
      if sty<0:
         sty='1'
      else:
         nbstyle=8
         sty=str((nbstyle+sty) % nbstyle)
      color=args['Coul']
      if color<0:
         color=str(args['NumSet'])
      else:
         nbcolor=15
         color=str((nbcolor+color) % nbcolor + 1)
      symbol=args['Marq']
      if symbol<0:
         symbol=str(args['NumSet'])
      else:
         nbsymbol=10
         symbol=str((nbsymbol+symbol) % nbsymbol + 1)
      freqm=args['FreqM']
      if freqm<0:
         freqm='0'
      else:
         freqm=str(freqm)

      sn=str(args['NumSet'])
      descr=[]
      descr.append(string.replace("""
@    s0 hidden false
@    s0 type xy
@    s0 symbol size 1.000000
@    s0 symbol pattern 1
@    s0 symbol linestyle 1
@    s0 symbol fill pattern 0
@    s0 symbol linewidth 1.0
@    s0 symbol char 65
@    s0 symbol char font 0
@    s0 line type 1
@    s0 line linewidth 1.0
@    s0 line pattern 1
@    s0 baseline type 0
@    s0 baseline off
@    s0 dropline off
@    s0 fill type 0
@    s0 fill rule 0
@    s0 fill pattern 1
@    s0 avalue off
@    s0 avalue type 2
@    s0 avalue char size 1.000000
@    s0 avalue font 0
@    s0 avalue rot 0
@    s0 avalue format general
@    s0 avalue prec 3
@    s0 avalue prepend ""
@    s0 avalue append ""
@    s0 avalue offset 0.000000 , 0.000000
@    s0 errorbar on
@    s0 errorbar place both
@    s0 errorbar pattern 1
@    s0 errorbar size 1.000000
@    s0 errorbar linewidth 1.0
@    s0 errorbar linestyle 1
@    s0 errorbar riser linewidth 1.0
@    s0 errorbar riser linestyle 1
@    s0 errorbar riser clip off
@    s0 errorbar riser clip length 0.100000

@    s0 comment ""
""",' s0 ',' s'+sn+' '))
      descr.append('@    s'+sn+' symbol '+symbol+'\n')
      descr.append('@    s'+sn+' symbol color '+color+'\n')
      descr.append('@    s'+sn+' symbol skip '+freqm+'\n')
      descr.append('@    s'+sn+' symbol fill color '+color+'\n')
      descr.append('@    s'+sn+' line linestyle '+sty+'\n')
      descr.append('@    s'+sn+' line color '+color+'\n')
      descr.append('@    s'+sn+' fill color '+color+'\n')
      descr.append('@    s'+sn+' avalue color '+color+'\n')
      descr.append('@    s'+sn+' errorbar color '+color+'\n')
      descr.append('@    s'+sn+' legend "'+args['Leg']+'"\n')
      return descr
#-------------------------------------------------
   def Trace(self):
      """
      Méthode pour 'tracer' l'objet Graph dans un fichier.
      Met en page l'entete, la description des courbes et les valeurs selon
      le format et ferme le fichier.
      """
      fich=self.Fich[0]
      g=self.Graph
      if g.NbCourbe < 1:
         fich.close()
         return
      # cohérence des valeurs par défaut
      if g.Grille_X<0 or g.Grille_Y<0:
         deltaX=g.Max_X-g.Min_X
         deltaY=g.Max_Y-g.Min_Y
         g.Grille_X=deltaX/5.
         g.Grille_Y=deltaY/5.
         if deltaX>4:
            g.Grille_X=int(round(g.Grille_X))
         if deltaY>4:
            g.Grille_Y=int(round(g.Grille_Y))
      # entete
      for lig in self.Entete():
         fich.write(lig)
      # valeurs
      it=-1
      for i in range(g.NbCourbe):
         dCi=g.Courbe(i)
         for k in range(dCi['NbCol']-1):
            it=it+1
            dCi['NumSet']=it
            for lig in self.DescrCourbe(**dCi):
               fich.write(lig)
      # partie données (.dat)
      it=-1
      for i in range(g.NbCourbe):
         dCi=g.Courbe(i)
         for k in range(dCi['NbCol']-1):
            it=it+1
            fich.write('@target g0.s'+str(it)+'\n')
            fich.write('@type xy'+'\n')
            for j in range(dCi['NbPts']):
               svX=self.DicForm['formR'] % dCi['Abs'][j]
               svY=self.DicForm['formR'] % dCi['Ord'][k][j]
               fich.write(svX+' '+svY+'\n')
            fich.write('&'+'\n')
      fich.close()
      
      # Production du fichier postscript, jpeg ou lancement interactif
      pilo=self.Pilote
      if self.Pilote!='':
         xmgr=os.path.join(aster.repout(),'xmgrace')
         nfhard=self.NomFich[0]+'.hardcopy'
         # nom exact du pilote
         if pilo=='POSTSCRIPT':
            pilo='PostScript'
         elif pilo=='INTERACTIF':
            pilo='X11'
         # ligne de commande
         if pilo=='X11':
            lcmde=xmgr+' '+self.NomFich[0]
         else:
            lcmde=xmgr+' -hardcopy -hdevice '+pilo+' -printfile '+nfhard+' '+self.NomFich[0]
         # appel xmgrace
         print ' <I> Lancement de : '+lcmde
         if not os.path.exists(xmgr):
            raise aster.error, ' <S> <ImprXmgrace> Fichier inexistant : '+xmgr
         iret=os.system(lcmde)
         if iret==0:
            if pilo not in ['','X11']:
               os.remove(self.NomFich[0])             # necessaire sous windows
               os.rename(nfhard,self.NomFich[0])
         else:
            raise aster.error, " <S> <ImprXmgrace> Erreur lors de l'utilisation du filtre "+pilo+"\nLe fichier retourné est le fichier '.agr'"
      return


#-------------------------------------------------
#-------------------------------------------------
#-------------------------------------------------
class ImprAgraf(ImprGraph):
   """
   Impression d'un objet Graph au format AGRAF.
   """
#-------------------------------------------------
   def Entete(self):
      """
      Retourne l'entete des directives Agraf
      """
      dic_ech={ 'LIN' : '0', 'LOG' : '1' }
      g=self.Graph
      entete=[]
      entete.append("""
ASPECT_GRAPHIQUE:
  En-tete :Departement Analyses Mecaniques et Acoustique
  Aspect :0
  Nombre de vues :1
  Cesure commentaire :40
  MinMax :0
  Fonte Titre :%helvetica-14
  Fonte Axes :%courier-12
  Fonte Autre :%times-12

  DEFAUT_COURBE:
    Couleur (rvb) :     0     0     0

  DEFAUT_COURBE:
    Couleur (rvb) : 65535     0     0

  DEFAUT_COURBE:
    Couleur (rvb) : 11822 35723 22359

  DEFAUT_COURBE:
    Couleur (rvb) :     0     0 65535

  DEFAUT_COURBE:
    Couleur (rvb) : 65535     0 65535

  DEFAUT_COURBE:
    Couleur (rvb) :     0 65535 65535

  DEFAUT_COURBE:
    Couleur (rvb) :     0 65535     0

  DEFAUT_COURBE:
    Couleur (rvb) : 41120 21074 11565

  DEFAUT_COURBE:
    Couleur (rvb) : 65535 42405     0

  DEFAUT_COURBE:
    Couleur (rvb) : 41120  8224 61680

  DEFAUT_COURBE:
    Couleur (rvb) : 65535 65535     0

  DEFAUT_COURBE:
    Couleur (rvb) : 53970 46260 35980

GRAPHIQUE:
""")
      if g.Titre=='':
         g.Titre='GRAPHIQUE CODE_ASTER'
      entete.append('Titre :'+g.Titre+'\n')
      if g.SousTitre!='':
         entete.append('Commentaire :'+g.SousTitre+'\n')
      entete.append('Frequence Grille X :'+str(int(g.Grille_X))+'\n')
      entete.append('Frequence Grille Y :'+str(int(g.Grille_Y))+'\n')
      entete.append('Echelle X :'+dic_ech[g.Echelle_X]+'\n')
      entete.append('Echelle Y :'+dic_ech[g.Echelle_Y]+'\n')
      if g.Legende_X!='':
         entete.append('Legende X :'+g.Legende_X+'\n')
      if g.Legende_Y!='':
         entete.append('Legende Y :'+g.Legende_Y+'\n')
      entete.append('Min X : '+str(g.Min_X)+'\n')
      entete.append('Max X : '+str(g.Max_X)+'\n')
      entete.append('Min Y : '+str(g.Min_Y)+'\n')
      entete.append('Max Y : '+str(g.Max_Y)+'\n')

      return entete
#-------------------------------------------------
   def DescrCourbe(self,**args):
      """
      Retourne la chaine de caractères décrivant les paramètres de la courbe.
      """
      sty=args['Sty']
      if sty<0:
         sty='0'
      else:
         nbstyle=2
         sty=str((nbstyle+sty) % nbstyle)
      color=args['Coul']
      if color<0:
         color=str(args['NumSet'])
      else:
         nbcolor=12
         color=str((nbcolor+color) % nbcolor)
      symbol=args['Marq']
      if symbol<0:
         symbol=str(args['NumSet'])
      else:
         nbsymbol=12
         symbol=str((nbsymbol+symbol) % nbsymbol)
      freqm=args['FreqM']
      if freqm<0:
         freqm='0'
      else:
         freqm=str(freqm)

      descr=[]
      descr.append('  COURBE:\n')
      descr.append('     Trait :'+sty+'\n')
      descr.append('     Couleur :'+color+'\n')
      descr.append('     Marqueur :'+symbol+'\n')
      descr.append('     Frequence Marqueur :'+freqm+'\n')
      if args['Leg']!='':
         descr.append('     Legende :'+args['Leg']+'\n')
      descr.append('     Tri :'+str(self.Graph.Tri)+'\n')
      descr.append('     Abscisses : [ '+str(args['Bloc'])+', '+str(args['ColX'])+']\n')
      descr.append('     Ordonnees : [ '+str(args['Bloc'])+', '+str(args['ColY'])+']\n')
      return descr
#-------------------------------------------------
   def Trace(self):
      """
      Méthode pour 'tracer' l'objet Graph dans un fichier.
      Met en page l'entete, la description des courbes et les valeurs selon
      le format et ferme le fichier.
      """
      fdogr=self.Fich[0]
      fdigr=self.Fich[1]
      g=self.Graph
      if g.NbCourbe < 1:
         fdogr.close()
         fdigr.close()
         return
      # cohérence des valeurs par défaut
      if g.Grille_X<0 or g.Grille_Y<0:
         g.Grille_X=0
         g.Grille_Y=0
      # entete
      for lig in self.Entete():
         fdigr.write(lig)
      # valeurs
      for i in range(g.NbCourbe):
         dCi=g.Courbe(i)
         dCi['NumSet']=i
         # partie directives (.digr)
         for k in range(dCi['NbCol']-1):
            dCi['Bloc']=i+1
            dCi['ColX']=1
            dCi['ColY']=k+2
            for lig in self.DescrCourbe(**dCi):
               fdigr.write(lig)
         # partie données (.dogr)
         fdogr.write('#NOM DE LA FONCTION: COURBE_'+str(i)+'\n')
         for j in range(dCi['NbPts']):
            for k in range(dCi['NbCol']):
               sv=self.DicForm['formR'] % g.Valeurs[i][k][j]
               fdogr.write(' '+sv)
            fdogr.write('\n')
      fdogr.close()
      fdigr.close()
      return
