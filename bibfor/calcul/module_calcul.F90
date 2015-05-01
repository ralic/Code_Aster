module module_calcul
implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
!    Ce module contient quelques variables "globales" utilisees
!    par la routine calcul.F90 ou pour les variables de commande
!
!======================================================================
    character(len=16) :: ca_option_, ca_nomte_, ca_nomtm_
!         ca_option_ : ca_option_ calculee
!         ca_nomte_  : type_element courant
!         ca_nomtm_  : type_maille associe au type_element courant
!
!======================================================================
    integer :: ca_igd_, ca_nec_, ca_ncmpmx_, ca_iachin_, ca_iachlo_, ca_iichin_
    integer :: ca_ilchlo_, ca_itypgd_, ca_ianueq_, ca_lprno_
    character(len=8) :: ca_typegd_
!     ca_igd_ : numero de la grandeur associee au champ a extraire
!     ca_nec_ : nombre d'entiers codes de ca_igd_
!     ca_ncmpmx_: nombre max de cmps pour ca_igd_
!     ca_iachin_: adresse jeveux de chin.vale
!     ca_iachlo_: adresse jeveux de chloc//".vale"  (&&calcul.nompar)
!     ca_ilchlo_: adresse jeveux de chloc//".exis"  (&&calcul.nompar)
!     ca_iichin_: numero du champ chin dans la liste lchin.
!     ca_ianueq_: adresse de l'objet .nueq du prof_chno associe eventuelle
!            -ment au champ chin. (si ca_lprno_=1).
!     ca_lprno_ : 1-> l'objet .nueq est a prendre en compte
!                 (cham_no a prof_chno)
!             0-> l'objet .nueq n'est pas a prendre en compte
!                 (cham_no a representation constante ou autre champ)
!     ca_typegd_: type scalaire de la grandeur ca_igd_ : 'R', 'I', 'K8', ...
!     ca_itypgd_: type scalaire de la grandeur ca_igd_ : 1,2,...
!             sert a eviter des comparaisons de chaines
!             la convention est ecrite dans extra1
!
!======================================================================
    integer :: ca_iaoptt_, ca_lgco_, ca_iaopmo_, ca_ilopmo_, ca_iaopno_, ca_ilopno_, ca_iaopds_
    integer :: ca_iaoppa_, ca_npario_, ca_nparin_, ca_iamloc_, ca_ilmloc_, ca_iadsgd_
!     ca_iaoptt_ : adresse de l'objet du catalogue : '&cata.te.optte'
!     ca_lgco_   : longueur d'une colonne de '&cata.te.optte'
!              ( nombre total d'options possibles du catalogue)
!     ca_iaopmo_ : adresse de '&cata.te.optmod'
!     ca_ilopmo_ : adresse du pt_long de '&cata.te.optmod'
!     ca_iaopno_ : adresse de '&cata.te.optnom'
!     ca_ilopno_ : adresse du pt_long de '&cata.te.optnom'
!     ca_iaopds_ : adresse de '&cata.op.descopt(opt)'
!     ca_iaoppa_ : adresse de '&cata.op.optpara(opt)'
!     ca_npario_ : longueur de '&cata.op.optpara(opt)'
!              = nb_param "in" + nb_param "out"
!     ca_nparin_ : nombre de parametres "in" pour l'ca_option_ opt.
!     ce nombre permet de savoir si un parametre est "in" ou "out"
!              (ipar <= ca_nparin_) <=> (ipar est "in")
!     ca_iamloc_ : adresse de '&cata.te.modeloc'
!     ca_ilmloc_ : adresse du pt_long de '&cata.te.modeloc'
!     ca_iadsgd_ : adresse de '&cata.gd.descrigd'
!
!======================================================================
    integer :: ca_iamaco_, ca_ilmaco_, ca_iamsco_, ca_ilmsco_, ca_ialiel_, ca_illiel_
!     ca_iamaco_  : adresse de la connectivite du maillage
!     ca_ilmaco_  : adresse du pointeur de longueur de ca_iamaco_
!     ca_iamsco_  : adresse de la connectivite des mailles suppl. d'1 ligrel
!     ca_ilmsco_  : adresse du pointeur de longueur de ca_iamsco_
!     ca_ialiel_  : adresse de l'objet '.liel' du ligrel.
!     ca_illiel_  : adresse du pointeur de longueur de '.liel'.
!
!======================================================================
    integer :: ca_iachii_, ca_iachik_, ca_iachix_
!     ca_iachii_ : adresse de '&&calcul.lchin_i'
!     ca_iachik_ : adresse de '&&calcul.lchin_k8'
!     ca_iachix_ : adresse de '&&calcul.lchin_exi'
!
!     '&&calcul.lchin_exi' ::= v(l)    (dim = nin)
!             v(1) :  .false.    : le champ parametre n'existe pas.
!
!     '&&calcul.lchin_k8'  ::= v(k8)    (dim = nin*2)
!             v(1) :  type_champ : 'chno','cart','chml' ou 'resl'.
!             v(2) :  type_gd    : 'c', 'r', 'i', 'k8', ...
!
!     '&&calcul.lchin_i'  ::= v(i)     (dim = nin*11)
!             v(1) :  ca_igd_   grandeur associee a lchin(i)
!             v(2) :  ca_nec_   nombre d'entiers codes
!             v(3) :  ca_ncmpmx_ nombre max de cmp pour ca_igd_
!             v(4) :  iadesc adresse de .desc  (ou .celd)
!             v(5) :  iavale
!                     si cham_no ou carte : adresse de .vale
!                     si cham_elem        : adresse de .celv
!             v(6) :  iaptma adresse de .ptma (pour 1 carte)
!             v(7) :  iaptms adresse de .ptms (pour 1 carte)
!             v(8) :  iaprn1 adresse du prno($mailla) (pour 1 cham_no)
!             v(9) :  iaprn2 adresse du prno(ligrel)  (pour 1 cham_no)
!             v(10):  ca_ianueq_ adresse    .nueq         (pour 1 cham_no)
!             v(11):  ca_lprno_  (dit si ca_ianueq_ est utilise pour 1 cham_no)
!
!======================================================================
    integer :: ca_ianoop_, ca_ianote_, ca_nbobtr_, ca_iaobtr_, ca_nbobmx_
!     IANOOP : ADRESSE DANS ZK16 DE '&&CALCUL.NOMOP' V(K16)
!          V(IOP) --> NOM DE L'OPTION IOP
!     IANOTE : ADRESSE DANS ZK16 DE '&&CALCUL.NOMTE' V(K16)
!          V(ITE) --> NOM DU TYPE_ELEMENT ITE
!          NBOBTR : NOMBRE D'OBJETS DE TRAVAIL '&&CALCUL....' QUI
!                   DEVRONT ETRE DETRUITS A LA FIN DE CALCUL.
!          IAOBTR : ADRESSE DANS ZK24 DE L'OBJET '&&CALCUL.OBJETS_TRAV'
!          NBOBMX : LONGUEUR DE L'OBJET '&&CALCUL.OBJETS_TRAV'
!
!======================================================================
    integer :: ca_nbgr_, ca_igr_, ca_nbelgr_, ca_jcteat_
    integer :: ca_lcteat_, ca_iawloc_, ca_iawlo2_, ca_iawtyp_
!
!
!     ca_nbgr_   : nombre de grel du ligrel
!     ca_igr_    : numero du grel courant
!     ca_nbelgr_ : nombre d'elements dans le grel ca_igr_
!
!     ca_jcteat_ : adresse dans zk16 de l'objet &cata.te.cte_attr(ca_nomte_)
!     ca_lcteat_ : longueur de l'objet &cata.te.cte_attr(ca_nomte_)
!       remarque : si ca_nomte_ n'a pas d'attribut : ca_jcteat_=ca_lcteat_=0
!
!  ca_iawloc_ : adresse dans zi de '&&calcul.ia_chloc' v(i)
!           cet objet contient des informations sur les champs locaux
!   v(3*(ipar-1)+1) : ca_iachlo_
!      adresse du champ_local '&&calcul.//nompar(ipar)
!      =-1 <=> / le champ "in" n'existe pas :
!                 / nompar n'appartient pas a lpain
!                 / chin//'.desc' (ou .celd) n'existe pas
!              / nompar n'appartient pas a lpaout
!      =-2 <=> aucun type_elem du ligrel ne declare nompar
!
!   v(3*(ipar-1)+2) : ca_ilchlo_ :
!      adresse d'un vecteur de booleens ( // champ_local)
!      de nom : '&&calcul.//nompar(ipar)//'.exis'
!      si ca_ilchlo_ = -1 :
!          =>  le champ local est "out"
!              et/ou le champ global n'existe pas
!              et/ou le parametre n'est pas utilise
!   v(3*(ipar-1)+3) : ich : numero du champ associe au parametre.
!      i.e : indice dans lchin (ou lchout selon le cas)
!      ich = 0 s'il n'y a pas de champ associe a ipar
!
!
!  ca_iawlo2_ : adresse dans zi de '&&calcul.ia_chlo2' v(i)
!           cet objet contient des informations sur les champs locaux
!   v(5*(ca_nbgr_*(ipar-1)+ca_igr_-1)+1):
!      mode local pour (ipar,ca_igr_)
!   v(5*(ca_nbgr_*(ipar-1)+ca_igr_-1)+2):
!      longueur du champ_local pour 1 element (lue dans le catalogue).
!      cette longueur ne tient pas compte de nbspt et ncdyn.
!      =-1 <=> le parametre n'est pas utilise par le type_element
!   v(5*(ca_nbgr_*(ipar-1)+ca_igr_-1)+3):
!      nombre de points de discretis. du champ_local
!      0 si resuelem
!   v(5*(ca_nbgr_*(ipar-1)+ca_igr_-1)+4):
!      longueur du champ local pour le grel ca_igr_
!   v(5*(ca_nbgr_*(ipar-1)+ca_igr_-1)+5):
!      adresse du debut du grel dans le champ_local (= 1 si ca_calvoi_=0)
!
!   ca_iawtyp_ : adresse dans zk8 de '&&calcul.type_sca' v(k8)
!          v(ipar) --> type_scalaire du champ_local
!
!======================================================================
    integer :: ca_iachoi_, ca_iachok_
!     ca_iachoi_ : adresse de '&&calcul.lchou_i'
!     ca_iachok_ : adresse de '&&calcul.lchou_k8'
!
!     '&&calcul.lchou_k8'  ::= v(k8)    (dim = nin*2)
!             v(1) :  type_champ : 'chml' ou 'resl'.
!             v(2) :  type_gd    : 'c', 'r'
!
!     '&&calcul.lchou_i'  ::= v(i)     (dim = nout*3)
!         -- si chml :
!             v(1) :  adresse de l_chout(i).celd
!             v(2) :  adresse de l_chout(i).celv
!         -- si resl :
!             v(1) :  adresse de l_chout(i).desc
!             v(2) :  adresse de l_chout(i).rsvi
!             v(3) :  adresse de loncum de l_chout(i).rsvi
!
!======================================================================
    integer :: ca_iel_
!     ca_iel_    : numero de l'element courant (dans le grel ca_igr_)
!         (ca_iel_ est mis a jour par extrai,te0000,montee,...)
!
!======================================================================
    integer :: ca_nbobj_, ca_iainel_, ca_ininel_
!     ca_nbobj_  : nombre d'objets '&inel.xxxx' cree par l'initialisation
!              du type_elem
!     ca_ininel_ : adresse dans zk24 de l'objet '&&calcul.nom_&inel'
!              qui contient les noms des objets '&inel.xxxx'
!     ca_iainel_ : adresse dans zi de l'objet '&&calcul.iad_&inel'
!              qui contient les adresses des objets '&inel.xxxx'
!
!======================================================================
    integer :: ca_icaeli_, ca_icaelk_
!     ca_icaelk_ est l'adresse d'un vecteur de k24 contenant :
!       v(1) : nom du maillage  (k8)
!       v(2) : nom du ligrel    (k19)
!       v(3) : nom de la maille    (k8)
!       v(3+  1) : nom du 1er noeud de la maille  (k8)
!       v(3+  i) : ...
!       v(3+nbno) : nom du der noeud de la maille  (k8)
!       v(3+nbno+1) : nom du type_element (k16)
!       v(3+nbno+2) : nom de l'ca_option_     (k16)
!     ca_icaeli_ est l'adresse d'un vecteur de is contenant :
!       v(1) : numero de la maille
!       v(2) : nombre de noeuds de la maille (nbno)
!       v(2+   1) : numero du 1er noeud de la maille
!       v(2+nbno) : numero du der noeud de la maille
!       v(2+nbno +1) : numero du grel
!       v(2+nbno +2) : numero de l'element dans le grel
!
!======================================================================
    integer :: ca_nute_, ca_jnbelr_, ca_jnoelr_, ca_iactif_, ca_jpnlfp_, ca_jnolfp_, ca_nblfpg_
!     ca_nute_ : numero du type_elem ca_nomte_.
!     ca_jnbelr_ : adresse dans zi  de '&cata.te.nbelrefe'
!     ca_jnoelr_ : adresse dans zk8 de '&cata.te.noelrefe'
!     ca_iactif_ :  1 : la routine calcul est active
!               0 : la routine calcul est inactive.
!               2 : la routine op0033 est active.
!         ce "booleen" permet d'arreter les utilisations des
!         routines qui doivent etre appellees "sous" les te00ij:
!         jevech,tecach,tecael,elref1,...
!     ca_jpnlfp_ : adresse dans zk32 de '&cata.te.pnlocfpg'
!     ca_jnolfp_ : adresse dans zi de '&cata.te.nolocfpg'
!     ca_nblfpg_ : dimension des objets pnlocfpg et nolocfpg
!======================================================================
    integer :: ca_caindz_(512), ca_capoiz_
!======================================================================
    integer :: ca_nbsav_
!======================================================================
    integer :: ca_nbcvrc_, ca_jvcnom_, ca_jvcfon_, ca_jvcval_
!     ca_nbcvrc_ : nombre de cvrc (variable de commande scalaire)
!     ca_jvcnom_ : adresse dans ZK8 des noms des cvrc
!     ca_jvcfon_ : adresse dans zk8 de '&&OP0033.TVCFON' (CALC_POINT_MAT)
!     ca_jvcval_ : adresse dans ZR  de '&&OP0033.TVCVAL' (CALC_POINT_MAT)
!======================================================================
    integer :: ca_nfpgmx_
    parameter (ca_nfpgmx_=10)
    integer :: ca_nfpg_, ca_jfpgl_, ca_decala_(ca_nfpgmx_), ca_km_, ca_kp_, ca_kr_, ca_iredec_
!     ca_nfpg_   : nombre de familles de la famille liste "mater"
!     ca_jfpgl_  : adresse dans zk8 de la liste des noms des familles
!     ca_decala_ : tableau des decalage des numeros des pg :
!         ca_decala_(1) : 0
!         ca_decala_(k) : nombre cumule des pg des familles (1:k-1)
!     ca_km_,ca_kp_,ca_kr_ : sauvegarde des numeros d'elements utilises dans
!                rcvarc. ils evitent des appels couteux a tecach.
!     ca_iredec_ : 0 : on n'est pas "sous" redece.f
!               1 : on est  "sous" redece.F90  (voir carr01 ci-dessous)
!======================================================================
    real(kind=8) :: ca_timed1_, ca_timef1_, ca_td1_, ca_tf1_
!     attention : il n'est pas valable si l'on n'est pas "sous" redece
!     ca_timed1_  : valeur de l'instant "-" du "gros" pas de temps
!     ca_timef1_  : valeur de l'instant "+" du "gros" pas de temps
!     td      : valeur de l'instant "-" du "petit" pas de temps
!     tf      : valeur de l'instant "+" du "petit" pas de temps
!======================================================================
    integer :: ca_evfini_, ca_calvoi_, ca_jrepe_, ca_jptvoi_, ca_jelvoi_
!
!     ca_evfini_  = 1 : le ligrel contient des "volumes finis"
!     ca_evfini_  = 0 : sinon
!     ca_calvoi_  = 1 : dans une routine te00ij, on veut pouvoir acceder
!                   aux champs des elements voisins (tecac2.f)
!     ca_calvoi_  = 0 : sinon
!
!     les 3 adresses suivantes sont remplies si ca_calvoi_=1 ou ca_evfini_=1 :
!      * ca_jrepe_   : adresse jeveux de ligrel.repe
!      * ca_jptvoi_  : adresse jeveux de maillage.vge.ptvois
!      * ca_jelvoi_  : adresse jeveux de maillage.vge.elvois
!
!
end module
