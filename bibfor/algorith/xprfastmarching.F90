subroutine xprfastmarching(cmnd, noma, fispre, cnxinv, noesom,&
                           lcmin, cnsln, grln, cnslt, grlt,&
                           isozro, nodtor,eletor, liggrd,&
                           vpoint, cnsbl ,deltat ,cnsbet ,listp )

   implicit none
!
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cescns.h"
#include "asterfort/cnscno.h"
#include "asterfort/getvr8.h"
#include "asterfort/dismoi.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeundf.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/xprls0.h"
#include "asterfort/xdecoupe.h"
#include "asterfort/xsolveurtria.h"
#include "asterfort/xvaleurmin.h"
#include "asterfort/xcalculgeo.h"
#include "asterfort/xprfastcalcul.h"

    character(len=8)  :: cmnd, noma, fispre
    character(len=19) :: cnsln, grln, cnslt, grlt, noesom, isozro
    character(len=19) :: nodtor, eletor, liggrd, cnxinv  
    character(len=19) :: cnsbl ,cnsbet ,listp , vpoint
    real(kind=8)      :: lcmin
    real(kind=8)      :: deltat
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: patrick.massin at edf.fr
!
!     ------------------------------------------------------------------
!
!       xprfastmarching : X-FEM PROPAGATION : REINITIALISATION ET
!                                             REORTHOGONALISATION DES LEVEL
!                                             SETS AVEC LA METHODE FAST MARCHING
!                                             SANS GRILLE AUXILIAIRE
!
!  DANS LE CADRE DE LA PROPAGATION X-FEM, UTILISATION DE LA METHODE
!  FAST MARCHING POUR LES PHASES DE REINITIALISATION DES LEVEL SETS APRES LA MISE A JOUR
!
!    ENTREE
!    ------
!      CMND   = 'REINITLN' POUR LA REINITIALISATION DE LA LEVEL SET
!                          NORMALE
!               'REINITLT' POUR LA REINITIALISATION DE LA LEVEL SET
!                          TANGENTE
!      NOMA   = NOM DU MAILLAGE DU MODELE
!      FISPRE = NOM DU CONCEPT FISSURE X-FEM DE LA FISSURE ACTUELLE
!      VCN    = VOIR XPRCNU.F POUR LA DESCRIPTION DE CETTE OBJET.
!      GRLR   = VOIR XPRCNU.F POUR LA DESCRIPTION DE CETTE OBJET.
!      NOESOM = VECTEUR LOGIQUE CONTENANT L'INFO 'NOEUD SOMMET'
!      LCMIN  = LONGEUR DE PLUS PETIT ARETE DU MAILLAGE NOMA
!      CNSLN  = CHAMP_NO_S DES VALEURS DE LA LEVEL SET NORMALE
!      GRLN   = CHAMP_NO_S DES VALEURS DU GRADIENT DE CNSLN
!      CNSLT  = CHAMP_NO_S DES VALEURS DE LA LEVEL SET TANGENTE
!      GRLT   = CHAMP_NO_S DES VALEURS DU GRADIENT DE CNSLT
!      ISOZRO = VECTEUR LOGIQUE INDIQUANT SI LA "VRAIE" LEVEL SET
!               (DISTANCE SIGNEE) A ETE CALCULEE POUR LE NOEUD
!      NODTOR = LISTE DES NOEUDS DEFINISSANTS LE DOMAINE DE CALCUL
!      ELETOR = LISTE DES ELEMENTS DEFINISSANTS LE DOMAINE DE CALCUL
!      LIGGRD = LIGREL DU DOMAINE DE CALCUL (VOIR XPRTOR.F)
!      CNSBET = VECTEUR DES ANGLES DE BIFURCATION DE LA FISSURE
!               EN CHAQUE POINT DU DOMAINE DE CALCUL (ANGLE AU POINT
!               PROJETE SUR LE FOND DE LA FISSURE)
!      LISTP  = VECTEUR (A 3 COMPOSANTES) OU LES CORDONNEES DU
!               PROJETE DE CHAQUE POINT DU DOMAINE DE CALCUL SUR LE
!               FOND DE LA FISSURE SONT STOCKEES
!    SORTIE
!    ------
!      CNSLN  = CHAMP_NO_S DES NOUVELLES VALEURS DE LA LEVEL SET NORMALE
!      GRLN   = CHAMP_NO_S DES NOUVELLES VALEURS DU GRADIENT DE CNSLN
!      CNSLT  = CHAMP_NO_S DES NOUVELLES VALEURS DE LA LEVEL SET
!               TANGENTE
!      GRLT   = CHAMP_NO_S DES NOUVELLES VALEURS DU GRADIENT DE CNSLT
!
!     ------------------------------------------------------------------

    character(len=19) :: cnsls, grls
    character(len=19) :: copiels, vtemp, calculs
    character(len=2)  :: levset

!     MESH INFORMATION RETREIVING AND GENERAL PURPOSE VARIABLES
    integer      :: nbno, nbnoma, jcnsls, jgrls
    integer      :: node , ndim
    integer      :: jbl, jbeta, jlistp, jvp, jltno
    integer      :: ifm, niv, jnodto, ibid
    integer      :: inar, jconx1, jconx2
    integer      :: jzero, jcopiels, jvtemp, jcalculs
    real(kind=8) :: newlsn, newlst
    
!     EVALUATION OF THE GRADIENT OF THE LEVEL SET
    character(len=8)  :: lpain(4), lpaout(2)
    character(len=19) :: cnols, celgls, chams
    character(len=24) :: lchin(4), lchout(2)
    real(kind=8), pointer       :: vale(:) => null()
    
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)

!    RETRIEVE THE LOCAL REFERENCE SYSTEM FOR EACH NODE IN THE MESH
    call jeveuo(cnsbl//'.CNSV', 'E', jbl)

!    GEOMETRIQUE
    call jeveuo(cnsbet, 'L', jbeta)
    call jeveuo(listp, 'L', jlistp)
    call jeveuo(vpoint, 'L', jvp)

!     RETRIEVE THE DIMENSION OF THE PROBLEM
    call dismoi('DIM_GEOM', noma, 'MAILLAGE', repi=ndim)

!     SET THE WORKING LEVEL SET FOR THE REINITIALIZATION PHASE
    levset = cmnd(7:8)
    if (levset .eq. 'LN') then
        call jeveuo(cnslt//'.CNSV', 'E', jltno)
        cnsls = cnsln
        grls = grln
    else
        cnsls = cnslt
        grls = grlt
    endif
!
!  RETRIEVE THE LEVEL SET AND ITS GRADIENT FOR THE INTEGRATION AT t=0
    call jeveuo(cnsls//'.CNSV', 'E', jcnsls)
    call jeveuo(grls//'.CNSV', 'E', jgrls)

    if (niv .ge. 0) then
        write(ifm,*) '   REINITIALISATION DE LA LEVEL SET ',levset,&
                       ' PAR LA METHODE FAST MARCHING'
    endif

!     RETRIEVE THE NUMBER OF NODES AND ELEMENTS IN THE MESH
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbnoma)

!     RETRIEVE THE NUMBER OF THE NODES THAT MUST TO BE USED IN THE
!     CALCULUS (SAME ORDER THAN THE ONE USED IN THE CONNECTION TABLE)
    call jeveuo(nodtor, 'L', jnodto)
!
!     RETRIEVE THE TOTAL NUMBER OF THE NODES THAT MUST BE ELABORATED
    call jelira(nodtor, 'LONMAX', nbno)

!     RETRIEVE THE COORDINATES OF THE NODES
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
        
!----------------------------------------------------------------------
!   CALCUL DES VRAIES DISTANCES SIGNEES SUR LES NOEUDS PROCHES DE LS=0
!   PERMET DE REPERER LES IZOZEROS DE LSN ET LST POUR LA FMM
!----------------------------------------------------------------------
    
    call wkvect(isozro, 'V V L', nbnoma, jzero)

    call xprls0(fispre, noma, noesom, lcmin, cnsln,&
                cnslt, isozro, levset, nodtor, eletor)

!----------------------------------------------------------------------
!                     BOUCLE LS INFERIEUR
!                        INITIALISATION
!----------------------------------------------------------------------
!creation d'une copie de ls :récupérer les valeurs de l'iso zéro pour la boucle supérieur
    copiels = '&&XPRFAST.COPIELS'
    call wkvect(copiels, 'V V R', nbnoma, jcopiels)

!creation d'un vecteur logique pour repérer les noeuds calculés
    vtemp = '&&XPRFAST.VTEMP'
    call wkvect(vtemp, 'V V L', nbnoma, jvtemp)

!creation d'un vecteur calculs qui modifira les valeurs de la ls
    calculs = '&&XPRFAST.CALCULS'
    call wkvect(calculs, 'V V R', nbnoma, jcalculs)
    
!multiplie par -1 pour calculer les valeurs negatives de ls
    zr(jcnsls:jcnsls+nbnoma-1) = -1 * zr(jcnsls:jcnsls+nbnoma-1)

!copie de ls
    zr(jcopiels:jcopiels+nbnoma-1) = zr(jcnsls:jcnsls+nbnoma-1)

!-----------------------------------------------------------------------
!            INITIALISATION AUTOUR DU FOND DE FISURE
!-----------------------------------------------------------------------    
    do inar = 1 , nbnoma
        zl(jvtemp-1+inar) = .false.
        zr(jcalculs-1+inar)= r8gaem()
    enddo

    do inar = 1 ,nbno
        node = zi(jnodto-1+inar)
        if ( zr(jcopiels-1+node) .ge. 0 ) then
            if (.not. zl(jzero-1+node)) then
                zl(jvtemp-1+node) = .true.         
            else
                if (levset .eq. 'LN' ) then
                    zl(jvtemp-1+node) = .true.
                    zr(jcalculs-1+node) = zr(jcopiels-1+node)
                else
                    call xcalculgeo(ndim, vale, jvp, jbl, deltat, jbeta, &
                                    jlistp, node, newlst, newlsn)                                 
                    zl(jvtemp-1+node) = .true.                    
                    zr(jcalculs-1+node) = -newlst
                endif
            endif
        endif
    end do

!  Propagation des valeurs a tout le domaine 
   call xprfastcalcul(jvtemp, nbnoma, jcalculs, jnodto, nbno, jcnsls, &
                      cnxinv, jconx1, jconx2, ndim, jcopiels, noma)

!----------------------------------------------------------------------
!                    BOUCLE LS SUPERIEUR
!                      INITIALISATION
!----------------------------------------------------------------------

!   On inverse les valeurs pour calculer l'autre cote de l'iso zero!
    zr(jcnsls:jcnsls+nbnoma-1)     = -1 * zr(jcnsls:jcnsls+nbnoma-1)
    zr(jcopiels:jcopiels+nbnoma-1) = -1 * zr(jcopiels:jcopiels+nbnoma-1)
    
!-----------------------------------------------------------------------
!              INITIALISATION AUTOUR DU FOND DE FISURE
!-----------------------------------------------------------------------
    do inar = 1 , nbnoma
        zl(jvtemp-1+inar) = .false.
        zr(jcalculs-1+inar)= r8gaem()
    enddo

    do inar = 1 ,nbno
        node = zi(jnodto-1+inar)
        if ( zr(jcopiels-1+node) .ge. 0 ) then
            if (.not. zl(jzero-1+node)) then            
                zl(jvtemp-1+node) = .true.
            else
                if (levset .eq. 'LN' ) then
                    zl(jvtemp-1+node) = .true.
                    zr(jcalculs-1+node) = zr(jcopiels-1+node)
                else
                    call xcalculgeo(ndim, vale, jvp, jbl, deltat, jbeta, &
                                    jlistp, node, newlst, newlsn)
                    zl(jvtemp-1+node) = .true.                    
                    zr(jcalculs-1+node) = newlst
                endif
            endif
        endif
    end do
    
!  Propagation des valeurs a tout le domaine    
   call xprfastcalcul(jvtemp, nbnoma, jcalculs, jnodto, nbno, jcnsls, &
                      cnxinv, jconx1, jconx2, ndim, jcopiels, noma)

!-----------------------------------------------------------------------
!     CALCUL DES GRADIENTS DES LEVEL SETS RESULTANTES
!-----------------------------------------------------------------------
!
!     DECLARE SOME DATA STRUCTURES FOR THE EVALUATION OF THE GRADIENT
    cnols = '&&XPRFMM.CNOLS'
    celgls = '&&XPRFMM.CELGLS'
    chams = '&&XPRFMM.CHAMS'

!   EVALUATION OF THE GRADIENT OF THE NEW LEVEL SET
    call cnscno(cnsls, ' ', 'NON', 'V', cnols,&
                'F', ibid)
    lpain(1)='PGEOMER'
    lchin(1)=noma//'.COORDO'
    lpain(2)='PNEUTER'
    lchin(2)=cnols
    lpaout(1)='PGNEUTR'
    lchout(1)=celgls
!
    call calcul('S', 'GRAD_NEUT_R', liggrd, 2, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
    call celces(celgls, 'V', chams)
    call cescns(chams, ' ', 'V', grls, ' ',&
                ibid)
    call jeveuo(grls//'.CNSV', 'E', jgrls)

!  DESTRUCTION DES OBJETS VOLATILES
    call jedetr(copiels)
    call jedetr(vtemp)
    call jedetr(calculs)
    call jedetr(cnols)
    call jedetr(celgls)
    call jedetr(chams)
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
