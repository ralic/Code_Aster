subroutine cuprep(mesh, nb_equa, ds_contact, disp_iter, time_curr)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterfort/caladu.h"
#include "asterfort/cudisi.h"
#include "asterfort/cusign.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
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
!
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: nb_equa
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: disp_iter
    real(kind=8), intent(in) :: time_curr
!
! --------------------------------------------------------------------------------------------------
!
! Unilateral constraint - Solve
!
! Prepare unilateral constraints
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  nb_equa          : number of equations
! In  ds_contact       : datastructure for contact management
! In  disp_iter        : displacement iteration
! In  time_curr        : current time
!
! --------------------------------------------------------------------------------------------------
!
    integer :: icmp, iret
    real(kind=8) :: coef, val, sign
    character(len=24) :: cmpgcu, coegcu, coedcu
    integer :: jcmpg, jcoefg, jcoefd
    character(len=24) :: apcoef, apjeu, poinoe, apddl, noeucu
    integer :: japcoe, japjeu, jpoi, japddl, jnoeu
    integer :: nnocu, ncmpg, inoe, nbddl, jdecal
    integer :: numnoe
    character(len=8) :: lispar(4)
    real(kind=8) :: valpar(4)
    real(kind=8), pointer :: coor(:) => null()
    real(kind=8), pointer :: depp(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES SD
!
    apcoef = ds_contact%sdunil_solv(1:14)//'.APCOEF'
    apjeu  = ds_contact%sdunil_solv(1:14)//'.APJEU'
    apddl  = ds_contact%sdunil_solv(1:14)//'.APDDL'
    noeucu = ds_contact%sdunil_defi(1:16)//'.LISNOE'
    call jeveuo(apjeu, 'E', japjeu)
    call jeveuo(apcoef, 'E', japcoe)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(noeucu, 'L', jnoeu)
!
! --- NOMBRE TOTAL DE DDLS ET NOMBRE TOTAL DE NOEUDS
!
    nnocu = cudisi(ds_contact%sdunil_defi,'NNOCU')
    ncmpg = cudisi(ds_contact%sdunil_defi,'NCMPG')
!
! --- EVALUATION DU MEMBRE DE DROITE (PSEUDO-JEU)
!
    coedcu = ds_contact%sdunil_defi(1:16)//'.COEFD'
    call jeveuo(coedcu, 'L', jcoefd)
    call jeveuo(mesh//'.COORDO    .VALE', 'L', vr=coor)
    lispar(1) = 'INST'
    lispar(2) = 'X'
    lispar(3) = 'Y'
    lispar(4) = 'Z'
    do inoe = 1, nnocu
        numnoe = zi(jnoeu-1+inoe)
        valpar(1) = time_curr
        valpar(2) = coor(1+3*(numnoe-1))
        valpar(3) = coor(1+3*(numnoe-1)+1)
        valpar(4) = coor(1+3*(numnoe-1)+2)
        call fointe('F', zk8(jcoefd-1+inoe), 4, lispar, valpar,&
                    coef, iret)
        zr(japjeu+inoe-1) = coef
    end do
!
! --- EVALUATION DU MEMBRE DE GAUCHE (COEFFICIENTS DE LA REL. LIN.)
!
    coegcu = ds_contact%sdunil_defi(1:16)//'.COEFG'
    call jeveuo(coegcu, 'L', jcoefg)
    cmpgcu = ds_contact%sdunil_defi(1:16)//'.CMPGCU'
    call jeveuo(cmpgcu, 'L', jcmpg)
!
    do icmp = 1, ncmpg
        call cusign(jcmpg, icmp, sign)
        call fointe('F', zk8(jcoefg-1+icmp), 1, ['INST'], [time_curr],&
                    coef, iret)
        zr(japcoe+icmp) = sign*coef
    end do
!
! --- CALCUL DE -A.DEPTOT ET RANGEMENT DANS APJEU
!
    poinoe = ds_contact%sdunil_defi(1:16)//'.POINOE'
    call jeveuo(poinoe, 'L', jpoi)
    call jeveuo(disp_iter(1:19)//'.VALE', 'E', vr=depp)
!
    do inoe = 1, nnocu
        jdecal = zi(jpoi+inoe-1)
        nbddl = zi(jpoi+inoe) - zi(jpoi+inoe-1)
        call caladu(nb_equa, nbddl, zr(japcoe+jdecal), zi(japddl+jdecal), depp,&
                    val)
        zr(japjeu+inoe-1) = zr(japjeu+inoe-1) - val
    end do
!
    call jedema()
!
end subroutine
