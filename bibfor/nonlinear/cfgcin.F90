subroutine cfgcin(resoco, matass, solveu, neq, nbliai)
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/calatm.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/r8inir.h"
#include "asterfort/resoud.h"
#include "blas/daxpy.h"
#include "blas/ddot.h"
    character(len=24) :: resoco
    integer :: neq, nbliai
    character(len=19) :: matass, solveu
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (RESOLUTION - GCP)
!
! INITIALISATION DE L'ALGORITHME
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  SOLVEU : SD SOLVEUR
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT
! IN  NEQ    : NOMBRE D'EQUATIONS
!
!
!
!
    real(kind=8) :: norme2
    integer :: iliai, jdecal, nbddl
    character(len=19) :: mu
    integer :: jmu
    character(len=24) :: apcoef, apddl, appoin
    integer :: japcoe, japddl, japptr
    character(len=24) :: secmbr, cncin0
    integer :: jsecmb
    character(len=19) :: ddeplc, ddelt
    complex(kind=8) :: c16bid
    character(len=19) :: k19bla
    integer :: iret
    real(kind=8), pointer :: vddelt(:) => null()
    real(kind=8), pointer :: ddepc(:) => null()
    c16bid = dcmplx(0.d0, 0.d0)
!
! ----------------------------------------------------------------------
!
    call jemarq()
    k19bla = ' '
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    appoin = resoco(1:14)//'.APPOIN'
    apcoef = resoco(1:14)//'.APCOEF'
    apddl = resoco(1:14)//'.APDDL'
    mu = resoco(1:14)//'.MU'
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apcoef, 'L', japcoe)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(mu, 'L', jmu)
!
! --- ACCES AUX CHAMPS DE TRAVAIL
! --- DDEPLC: INCREMENT DE SOLUTION APRES CORRECTION DU CONTACT
! --- DDELT : INCREMENT DE SOLUTION ITERATION DE CONTACT
!
    ddeplc = resoco(1:14)//'.DELC'
    ddelt = resoco(1:14)//'.DDEL'
    cncin0 = resoco(1:14)//'.CIN0'
    secmbr = resoco(1:14)//'.SECM'
    call jeveuo(ddeplc(1:19)//'.VALE', 'E', vr=ddepc)
    call jeveuo(secmbr(1:19)//'.VALE', 'E', jsecmb)
!
! --- INITIALISATION A PARTIR DU CHAMP DE MULTIPLICATEURS INITIAL MU
! --- S'IL EST NON-NUL
!
    norme2 = ddot(nbliai,zr(jmu),1,zr(jmu),1)
!
    if (norme2 .ne. 0.d0) then
!
! ----- CALCUL DU SECOND MEMBRE
!
        call r8inir(neq, 0.d0, zr(jsecmb), 1)
        do iliai = 1, nbliai
            jdecal = zi(japptr+iliai-1)
            nbddl = zi(japptr+iliai) - zi(japptr+iliai-1)
            call calatm(neq, nbddl, zr(jmu+iliai-1), zr(japcoe+jdecal), zi(japddl+jdecal),&
                        zr(jsecmb))
        end do
!
! ----- RESOLUTION
!
        call resoud(matass, k19bla, solveu, cncin0, 0,&
                    secmbr, ddelt, 'V', [0.d0], [c16bid],&
                    k19bla, .true._1, 0, iret)
!
! ----- U = U + (-DELTA)
!
        call jeveuo(ddelt(1:19) //'.VALE', 'L', vr=vddelt)
        call daxpy(neq, -1.d0, vddelt, 1, ddepc,&
                   1)
    endif
!
    call jedema()
!
end subroutine
