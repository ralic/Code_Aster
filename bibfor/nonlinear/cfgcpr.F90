subroutine cfgcpr(resoco, matass, solveu, neq, nbliai,&
                  search, alpha)
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
    implicit none
#include "jeveux.h"
#include "asterfort/calatm.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/r8inir.h"
#include "asterfort/resoud.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    character(len=24) :: resoco
    character(len=16) :: search
    integer :: neq, nbliai
    character(len=19) :: matass, solveu
    real(kind=8) :: alpha
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (RESOLUTION - GCP)
!
! PROJECTION DU PAS D'AVANCEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  SOLVEU : SD SOLVEUR
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  SEARCH : RECHERCHE LINEAIRE
!              'ADMISSIBLE'
!              'NON_ADMISSIBLE'
! I/O ALPHA  : COEFFICIENT DE RECHERCHE LINEAIRE
!
!
!
!
    integer :: ifm, niv
    integer :: iliai, jdecal, nbddl
    complex(kind=8) :: c16bid
    character(len=19) :: k19bla
    character(len=24) :: apcoef, apddl, appoin
    integer :: japcoe, japddl, japptr
    character(len=19) :: direct
    integer :: jdirec
    character(len=19) :: ddeplc, ddepl0, ddelt
    character(len=24) :: secmbr, cncin0
    integer :: jsecmb
    character(len=19) :: mu
    integer :: jmu
    integer :: iret
    real(kind=8), pointer :: vddelt(:) => null()
    real(kind=8), pointer :: ddep0(:) => null()
    real(kind=8), pointer :: ddepc(:) => null()
    c16bid = dcmplx(0.d0, 0.d0)
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
    k19bla = ' '
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    appoin = resoco(1:14)//'.APPOIN'
    apcoef = resoco(1:14)//'.APCOEF'
    apddl = resoco(1:14)//'.APDDL'
    direct = resoco(1:14)//'.DIRE'
    mu = resoco(1:14)//'.MU'
!
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apcoef, 'L', japcoe)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(direct, 'L', jdirec)
    call jeveuo(mu, 'E', jmu)
!
! --- ACCES AUX CHAMPS DE TRAVAIL
! --- DDEPL0: INCREMENT DE SOLUTION SANS CORRECTION DU CONTACT
! --- DDEPLC: INCREMENT DE SOLUTION APRES CORRECTION DU CONTACT
! --- DDELT : INCREMENT DE SOLUTION ITERATION DE CONTACT
!
    ddepl0 = resoco(1:14)//'.DEL0'
    ddeplc = resoco(1:14)//'.DELC'
    ddelt = resoco(1:14)//'.DDEL'
    call jeveuo(ddepl0(1:19)//'.VALE', 'L', vr=ddep0)
    call jeveuo(ddeplc(1:19)//'.VALE', 'E', vr=ddepc)
    call jeveuo(ddelt (1:19)//'.VALE', 'E', vr=vddelt)
!
! --- ACCES AUX CHAMPS DE TRAVAIL
!
    secmbr = resoco(1:14)//'.SECM'
    cncin0 = resoco(1:14)//'.CIN0'
    call jeveuo(secmbr(1:19)//'.VALE', 'E', jsecmb)
!
! --- RECALCUL DE ALPHA POUR UNE SOLUTION ADMISSIBLE
!
    if (search .eq. 'ADMISSIBLE') then
        do iliai = 1, nbliai
            if (zr(jdirec-1+iliai) .lt. 0.d0) then
                alpha = min(alpha,-zr(jmu+iliai-1)/zr(jdirec-1+iliai))
            endif
        end do
        if (niv .eq. 2) then
            write (ifm,9050) alpha
        endif
    endif
!
! --- MISE A JOUR DE MU
!
    call daxpy(nbliai, alpha, zr(jdirec), 1, zr(jmu),&
               1)
!
! --- DESACTIVATION DE MU POUR UNE SOLUTION NON-ADMISSIBLE
!
    if (search .eq. 'NON_ADMISSIBLE') then
        do iliai = 1, nbliai
            if (zr(jmu-1+iliai) .lt. 0.d0) then
                zr(jmu-1+iliai) = 0.d0
            endif
        end do
    endif
!
! --- RECALCUL D'UN SOLUTION
!
    if (search .eq. 'NON_ADMISSIBLE') then
!
        call r8inir(neq, 0.d0, zr(jsecmb), 1)
        call r8inir(neq, 0.d0, vddelt, 1)
!
! ----- SECOND MEMBRE: [A]T.{MU}
!
        do iliai = 1, nbliai
            jdecal = zi(japptr+iliai-1)
            nbddl = zi(japptr+iliai) - zi(japptr+iliai-1)
            call calatm(neq, nbddl, zr(jmu-1+iliai), zr(japcoe+jdecal), zi(japddl+jdecal),&
                        zr(jsecmb))
        end do
!
! ----- RESOLUTION [K].{DDELT} = [A]T.{MU} -> {DDELT}
!
        call resoud(matass, k19bla, solveu, cncin0, 0,&
                    secmbr, ddelt, 'V', [0.d0], [c16bid],&
                    k19bla, .true._1, 0, iret)
!
! ----- RECOPIE DE LA SPOLUTION SANS CONTACT
!
        call dcopy(neq, ddep0, 1, ddepc, 1)
        alpha = 1.d0
    endif
!
    9050 format (' <CONTACT><CALC> PAS D''AVANCEMENT APRES PROJECTION : ',&
     &       1pe12.5)
!
    call jedema()
!
end subroutine
