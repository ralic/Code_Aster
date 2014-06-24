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
# person_in_charge: mathieu.courtois at edf.fr

IMPR_FONCTION=MACRO(nom="IMPR_FONCTION",
                    op=OPS('Macro.impr_fonction_ops.impr_fonction_ops'),
                    sd_prod=None,
                    fr=tr("Imprime le contenu d'objets de type fonction ou liste de "
                         "réels dans un fichier destiné à un traceur de courbe"),
                    UIinfo={"groupes":("Impression","Fonctions",)},
         FORMAT          =SIMP(statut='o',typ='TXM',position='global',defaut='TABLEAU',
                               into=("TABLEAU","AGRAF","XMGRACE",),),
         b_pilote = BLOC(condition = "FORMAT == 'XMGRACE'",
                        fr=tr("Mots-clés propres à XMGRACE"),
           PILOTE          =SIMP(statut='f',typ='TXM',defaut='',
                                 into=('','POSTSCRIPT','EPS','MIF','SVG','PNM','PNG','JPEG','PDF','INTERACTIF'),
                            fr=tr("Pilote de sortie, PNG/JPEG/PDF ne sont pas disponibles sur toutes les installations de xmgrace")),
           UNITE           =SIMP(statut='f',typ='I',val_min=10,val_max=90,defaut=29,
                                 fr=tr("Unité logique définissant le fichier (fort.N) dans lequel on écrit")),
         ),
         b_agraf = BLOC(condition = "FORMAT == 'AGRAF'",
                        fr=tr("Mots-clés propres à AGRAF"),
           UNITE           =SIMP(statut='o',typ='I',defaut=25,
                                 fr=tr("Unité logique définissant le fichier (fort.N) dans lequel on écrit")),
           UNITE_DIGR      =SIMP(statut='o',typ='I',defaut=26,
                                 fr=tr("Unité logique définissant le fichier dans lequel on écrit les directives Agraf")),
         ),
         # unite pour TABLEAU dans le bloc de mise en forme spécifique

         COURBE          =FACT(statut='o',max='**',fr=tr("Définition de la fonction à tracer"),
           regles=(UN_PARMI('FONCTION','LIST_RESU','FONC_X','ABSCISSE'),),
           FONCTION        =SIMP(statut='f',typ=(fonction_sdaster, formule, fonction_c, nappe_sdaster),
                                 fr=tr("Fonction réelle ou complexe"), ),
           LIST_RESU       =SIMP(statut='f',typ=listr8_sdaster,
                                 fr=tr("Liste des ordonnees d'une fonction réelle définie par deux listes"), ),
           FONC_X          =SIMP(statut='f',typ=(fonction_sdaster,formule),
                                 fr=tr("Fonction abscisses d'une fonction paramétrique"),),
           ABSCISSE        =SIMP(statut='f',typ='R',max='**',
                                 fr=tr("Valeurs des abscisses"), ),
           b_fonction      =BLOC(condition = "FONCTION != None",
             LIST_PARA       =SIMP(statut='f',typ=listr8_sdaster ),
           ),
           b_fonction_c  =BLOC(condition = "AsType(FONCTION) in (fonction_c, formule_c)",
                                 fr=tr("Fonction complexe définie par le mot-clé fonction"),
             PARTIE          =SIMP(statut='f',typ='TXM',into=("REEL","IMAG") ),
           ),
           b_list_resu     =BLOC(condition = "LIST_RESU != None",
             LIST_PARA       =SIMP(statut='o',typ=listr8_sdaster ),
           ),
           b_fonc_x        =BLOC(condition = "FONC_X != None",
             FONC_Y          =SIMP(statut='o',typ=(fonction_sdaster,formule),
                                   fr=tr("Fonction ordonnées d une fonction paramétrique") ),
             LIST_PARA       =SIMP(statut='f',typ=listr8_sdaster ),
           ),
           b_vale_resu     =BLOC(condition = "ABSCISSE != None",
             ORDONNEE      =SIMP(statut='o',typ='R',max='**',
                                 fr=tr("Valeurs des ordonnées")),
           ),

           # mots-clés utilisant uniquement aux formats autres que TABLEAU
           # mais ce serait trop pénible de devoir les supprimer quand on change de format
           # donc on ne les met pas dans un bloc
           # "pseudo" bloc mise en forme :
               LEGENDE         =SIMP(statut='f',typ='TXM',
                                    fr=tr("Légende associée à la fonction") ),
               STYLE           =SIMP(statut='f',typ='I',val_min=0,
                                    fr=tr("Style de la ligne représentant la fonction"),),
               COULEUR         =SIMP(statut='f',typ='I',val_min=0,
                                    fr=tr("Couleur associée à la fonction"),),
               MARQUEUR        =SIMP(statut='f',typ='I',val_min=0,
                                    fr=tr("Type du marqueur associé à la fonction"),),
               FREQ_MARQUEUR   =SIMP(statut='f',typ='I',defaut=0,
                                    fr=tr("Fréquence d impression du marqueur associé à la fonction"), ),
           # fin bloc mise en forme

           TRI             =SIMP(statut='f',typ='TXM',defaut="N",
                                 fr=tr("Choix du tri effectué sur les abscisses ou sur les ordonnées"),
                                 into=("N","X","Y","XY","YX") ),
         ),
         # Mise en page du tableau ou du graphique
         b_tableau = BLOC(condition = "FORMAT == 'TABLEAU'",
                          fr=tr("Mots-clés propres au format Tableau"),
           UNITE           =SIMP(statut='o',typ='I',defaut=8,
                                 fr=tr("Unité logique définissant le fichier (fort.N) dans lequel on écrit")),
           TITRE           =SIMP(statut='f',typ='TXM',
                                 fr=tr("Titre associé au graphique") ),
           SOUS_TITRE      =SIMP(statut='f',typ='TXM',
                                 fr=tr("Sous-titre du graphique") ),
           SEPARATEUR      =SIMP(statut='f',typ='TXM',defaut=' ',
                                 fr=tr("Séparateur des colonnes du tableau (ex : ' ', ';'...)")),
           COMMENTAIRE     =SIMP(statut='f',typ='TXM',defaut='#',
                                 fr=tr("Caractère indiquant au traceur de fonction que la ligne peut etre ignorée")),
           COMM_PARA       =SIMP(statut='f',typ='TXM',defaut='',
                                 fr=tr("Caractère utilisé pour commentariser la ligne des labels de colonnes")),
           DEBUT_LIGNE     =SIMP(statut='f',typ='TXM',defaut='',
                                 fr=tr("Caractère de debut de ligne")),
           FIN_LIGNE       =SIMP(statut='f',typ='TXM',defaut='\n',
                                 fr=tr("Caractère de fin de ligne")),
         ),
         b_graphique = BLOC(condition = "FORMAT != 'TABLEAU'",
                        fr=tr("Mise en page du graphique"),
           TITRE           =SIMP(statut='f',typ='TXM',
                                 fr=tr("Titre associé au graphique") ),
           SOUS_TITRE      =SIMP(statut='f',typ='TXM',
                                 fr=tr("Sous-titre du graphique") ),
           BORNE_X         =SIMP(statut='f',typ='R',min=2,max=2,
                                 fr=tr("Intervalles de variation des abscisses")),
           BORNE_Y         =SIMP(statut='f',typ='R',min=2,max=2,
                                 fr=tr("Intervalles de variation des ordonnées")),
           ECHELLE_X       =SIMP(statut='f',typ='TXM',defaut="LIN",into=("LIN","LOG"),
                                 fr=tr("Type d'échelle pour les abscisses") ),
           ECHELLE_Y       =SIMP(statut='f',typ='TXM',defaut="LIN",into=("LIN","LOG"),
                                 fr=tr("Type d'échelle pour les ordonnées") ),
           GRILLE_X        =SIMP(statut='f',typ='R',max=1,val_min=0.,
                                 fr=tr("Pas du quadrillage vertical") ),
           GRILLE_Y        =SIMP(statut='f',typ='R',max=1,val_min=0.,
                                 fr=tr("Pas du quadrillage horizontal") ),
           LEGENDE_X       =SIMP(statut='f',typ='TXM',
                                 fr=tr("Légende associée à l'axe des abscisses") ),
           LEGENDE_Y       =SIMP(statut='f',typ='TXM',
                                 fr=tr("Légende associée à l'axe des ordonnées") ),
         ),
         INFO            =SIMP(statut='f',typ='I',defaut=1,into=(1,2) ),
)  ;
