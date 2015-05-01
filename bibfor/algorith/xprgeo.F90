subroutine xprgeo(noma, cnsln, cnslt, grln, grlt,&
                  vpoint, cnsbl, deltat, nodtor, liggrd,&
                  cnsbet, listp, operation)
!
! aslint: disable=
    implicit none
!
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cescns.h"
#include "asterfort/cnscno.h"
#include "asterfort/dismoi.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    real(kind=8) :: deltat
    character(len=8) :: noma
    character(len=16) :: operation
    character(len=19) :: cnsln, cnslt, grln, grlt, cnsbl, nodtor, liggrd, cnsbet
    character(len=19) :: listp, vpoint
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: daniele.colombo at ifpen.fr
!
!
!     ------------------------------------------------------------------
!
!       XPRGEO   : X-FEM PROPAGATION GEOMETRIQUE DES LEVEL SETS
!       ------     -     --          ---
!    PROPAGATION DES LEVEL SETS AU PAS DE TEMP SUIVANT FAIT PAR
!    UN ALGORITHME GEOMETRIQUE
!
!    ENTREE
!        NOMA    : NOM DU CONCEPT MAILLAGE
!        CNSLT   : CHAM_NO_S LEVEL SET TANGENTIELLE
!        CNSLN   : CHAM_NO_S LEVEL SET NORMALE
!        GRLT    : CHAM_NO_S GRADIENT DE LEVEL SET TANGENTIELLE
!        GRLN    : CHAM_NO_S GRADIENT DE LEVEL SET NORMALE
!        VPOINT  : VECTEUR DES VITESSES DE PROPAGATION EN CHAQUE POINT
!                  DU DOMAINE DE CALCUL (MODULE DE LA VITESSE DU POINT
!                  PROJETE SUR LE FOND DE LA FISSURE)
!        CNSBL   : CHAM_NO_S DES VECTEURS NORMALE ET TANGENTIELLE DE LA
!                  BASE LOCALE IN CHAQUE NODE DU MAILLAGE
!        DELTAT  : TEMPS TOTAL DU PAS DE PROPAGATION
!        NODTOR  : LISTE DES NOEUDS DEFINISSANT LE DOMAINE DE CALCUL
!        LIGGRD  : LIGREL DU DOMAINE DE CALCUL (VOIR XPRTOR.F)
!        CNSBET  : VECTEUR DES ANGLES DE BIFURCATION DE LA FISSURE
!                  EN CHAQUE POINT DU DOMAINE DE CALCUL (ANGLE AU POINT
!                  PROJETE SUR LE FOND DE LA FISSURE)
!        LISTP   : VECTEUR (A 3 COMPOSANTES) OU LES CORDONNEES DU
!                  PROJETE DE CHAQUE POINT DU DOMAINE DE CALCUL SUR LE
!                  FOND DE LA FISSURE SONT STOCKEES
!
!    SORTIE
!        CNSLT   : CHAM_NO_S LEVEL SET TANGENTIELLE
!        CNSLN   : CHAM_NO_S LEVEL SET NORMALE
!        GRLT    : CHAM_NO_S GRADIENT DE LEVEL SET TANGENTIELLE
!        GRLN    : CHAM_NO_S GRADIENT DE LEVEL SET NORMALE
!
!     ------------------------------------------------------------------
!
!
    integer :: i, ifm, niv, nbno,   ndim, j, jnodto, node, ier
    integer :: ibid,  jbeta, jlistp,  pos, pos1, jvp
    character(len=8) :: lpain(2), lpaout(1)
    character(len=19) :: chgrlt, chgrln, chams, cnolt, cnoln
    character(len=24) :: lchin(2), lchout(1)
    real(kind=8) :: t1(3), n1(3), p1(3), deltaa, newlsn, newlst, cbeta, sbeta
    real(kind=8), pointer :: vale(:) => null()
    real(kind=8), pointer :: bl(:) => null()
    real(kind=8), pointer :: lnno(:) => null()
    real(kind=8), pointer :: ltno(:) => null()
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
!     RETRIEVE THE LOCAL REFERENCE SYSTEM FOR EACH NODE IN THE MESH
    call jeveuo(cnsbl//'.CNSV', 'E', vr=bl)
!
!     RETRIEVE THE DIMENSION OF THE PROBLEM (2D AND 3D ARE SUPPORTED)
    call dismoi('DIM_GEOM', noma, 'MAILLAGE', repi=ndim)
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
!
!     RETRIEVE THE NUMBER OF THE NODES THAT MUST TO BE USED IN THE
!     CALCULUS (SAME ORDER THAN THE ONE USED IN THE CONNECTION TABLE)
    call jeveuo(nodtor, 'L', jnodto)
!
!     RETRIEVE THE TOTAL NUMBER OF THE NODES THAT MUST BE ELABORATED
    call jelira(nodtor, 'LONMAX', nbno)
!
    call jeveuo(cnsbet, 'L', jbeta)
    call jeveuo(listp, 'L', jlistp)
    call jeveuo(vpoint, 'L', jvp)
!
! ***************************************************************
! UPDATE THE LEVEL SETS
! ***************************************************************
!
!     RECUPERATION DE L'ADRESSE DES VALEURS DE LT ET LN
    call jeveuo(cnslt//'.CNSV', 'E', vr=ltno)
    call jeveuo(cnsln//'.CNSV', 'E', vr=lnno)
!
!     UPDATE THE LEVEL SETS FOR EACH NODE IN THE TORE
    do i = 1, nbno
!
!         RETREIVE THE NODE NUMBER
        node = zi(jnodto-1+i)
!
!         PROPAGATION VECTOR DELTA_A
        if(operation.eq.'PROPA_COHESIF'.or.operation.eq.'DETECT_COHESIF') then
            deltaa=zr(jvp-1+node)
        else 
            deltaa=zr(jvp-1+node)*deltat
        endif
!
!         STORE THE COS AND SIN OF THE PROPAGATION ANGLE
        cbeta = cos(zr(jbeta-1+node))
        sbeta = sin(zr(jbeta-1+node))
!
!         POINTERS INSIDE THE JEVEUX OBJECTS
        pos = 2*ndim*(node-1)
        pos1 = 3*(node-1)
!
!         RESET THE NEW VALUE OF THE TWO LEVEL SETS
        newlsn = 0.d0
        newlst = 0.d0
!
        do j = 1, ndim
!            NEW T-AXIS BY A RIGID ROTATION AT THE NEW CRACK TIP
            t1(j) = cbeta*bl(pos+ndim+j)+sbeta*bl(pos+j)
!            NEW N-AXIS BY A RIGID ROTATION AT THE NEW CRACK TIP
            n1(j) = cbeta*bl(pos+j)-sbeta*bl(pos+ndim+j)
!            NEW CRACK TIP POSITION
            p1(j) = zr(jlistp-1+pos1+j)+deltaa*t1(j)
!            NEW VALUES OF THE TWO LEVEL SETS
            newlsn = newlsn+(vale(pos1+j)-p1(j))*n1(j)
            newlst = newlst+(vale(pos1+j)-p1(j))*t1(j)
        end do
!
!         MODIFY THE NORMAL LEVEL SET ONLY IN THE POINTS WHERE THE
!         TANGENTIAL LEVEL SET IS POSITIVE
        if (ltno(node) .gt. 0.d0.and.operation.ne.'DETECT_COHESIF') then
            lnno(node) = newlsn
        endif
!
!         STORE THE NEW VALUE OF THE TANTENGIAL LEVEL SET
        ltno(node) = newlst
!
    end do
!
!-----------------------------------------------------------------------
!     CALCUL DES GRADIENTS DES LEVEL SETS RESULTANTES
!-----------------------------------------------------------------------
!
!     CREATION DES OBJETS VOLATILES
    chgrlt = '&&XPRLS.CHGRLT'
    chgrln = '&&XPRLS.CHGRLN'
    chams = '&&XPRLS.CHAMS'
    cnolt = '&&XPRLS.CNOLT'
    cnoln = '&&XPRLS.CNOLN'
!
!  GRADIENT DE LT
    call cnscno(cnslt, ' ', 'NON', 'V', cnolt,&
                'F', ibid)
!
    lpain(1) = 'PGEOMER'
    lchin(1) = noma//'.COORDO'
    lpain(2) = 'PNEUTER'
    lchin(2) = cnolt
    lpaout(1)= 'PGNEUTR'
    lchout(1)= chgrlt
!
    call calcul('S', 'GRAD_NEUT_R', liggrd, 2, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
    call celces(chgrlt, 'V', chams)
    call cescns(chams, ' ', 'V', grlt, ' ',&
                ier)
!
!  GRADIENT DE LN
    if(operation.ne.'DETECT_COHESIF') then
        call cnscno(cnsln, ' ', 'NON', 'V', cnoln,&
        'F', ibid)
!
        lpain(1) = 'PGEOMER'
        lchin(1) = noma//'.COORDO'
        lpain(2) = 'PNEUTER'
        lchin(2) = cnoln
        lpaout(1)= 'PGNEUTR'
        lchout(1)= chgrln
!
        call calcul('S', 'GRAD_NEUT_R', liggrd, 2, lchin,&
        lpain, 1, lchout, lpaout, 'V',&
        'OUI')
!
        call celces(chgrln, 'V', chams)
        call cescns(chams, ' ', 'V', grln, ' ',&
        ier)
!
!  DESTRUCTION DES OBJETS VOLATILES
        call jedetr(chgrln)
        call jedetr(cnoln)
    endif
    call jedetr(chgrlt)
    call jedetr(chams)
    call jedetr(cnolt)
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
