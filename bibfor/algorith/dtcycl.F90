subroutine dtcycl(vecdt, nitnew, icycl, dtmin)
    implicit none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
! DESCRIPTION : PROCEDURE DE DETECTION D'ITERATIONS CYCLIQUES
! -----------   DANS NEWTON
!
!               APPELANT : NEWTON
!
! IN     : VECDT  : REAL*8 , VECTEUR DE DIMENSION NITMAX+1 = 151
!                   VECTEUR DE TRAVAIL CONTENANT LES PAS DE TEMPS
!                   AJUSTES PAR NEWTON A CHAQUE ITERATION
! IN     : NITNEW : INTEGER , SCALAIRE
!                   NOMBRE D'ITERATIONS DEJA EXECUTEES DANS NEWTON
!
! OUT    : ICYCL  : INTEGER , SCALAIRE
!                   INDICATEUR D'ITERATIONS CYCLIQUES
!                   ICYCL = 0 : PAS D'ITERATIONS CYCLIQUES
!                   ICYCL = 1 : ITERATIONS CYCLIQUES
! OUT    : DTMIN  : REAL*8 , SCALAIRE
!                   VALEUR MINIMALE DU PAS DE TEMPS AU COURS DES
!                   ITERATIONS CYCLIQUES
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    real(kind=8) :: vecdt(*)
    integer :: nitnew, icycl
    real(kind=8) :: dtmin
!
! VARIABLES LOCALES
! -----------------
    integer :: idt, idt1, idt2
    real(kind=8) :: dtcour, dtref, epsdtc
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  ABS
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    icycl = 0
!.... N.B. EPSDTC EST DEFINIE EN COHERENCE AVEC NEWTON
    epsdtc = 1.0d-04
!
    do 10 idt1 = 0, nitnew-1
        dtref = vecdt(idt1+1)
        do 11 idt2 = idt1+1, nitnew
            dtcour = vecdt(idt2+1)
            if ((abs(dtcour-dtref)/dtref) .lt. epsdtc) then
                icycl = 1
                goto 20
            endif
11      continue
10  end do
!
20  continue
    if (icycl .eq. 1) then
        dtmin = dtref
        if ((idt2-idt1) .gt. 1) then
            do 30 idt = idt1+1, idt2-1
                if (vecdt(idt+1) .lt. dtmin) dtmin = vecdt(idt+1)
30          continue
        endif
    endif
!
! --- FIN DE DTCYCL.
end subroutine
