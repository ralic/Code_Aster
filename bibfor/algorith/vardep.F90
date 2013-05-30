subroutine vardep(nbnl, dep, dep0, tconf2, tconf1,&
                  ivar, dt0, toln, tolc, tolv)
    implicit none
!-----------------------------------------------------------------------
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
! DESCRIPTION : TEST SUR LA VARIATION DU DEPLACEMENT PHYSIQUE
! -----------   ENTRE LES INSTANTS N ET N+1
!
!               APPELANT : MDITM2
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    integer :: nbnl
    real(kind=8) :: dep(3, *), dep0(3, *), tconf2(4, *), tconf1(4, *)
    integer :: ivar
    real(kind=8) :: dt0, toln, tolc, tolv
!
! VARIABLES LOCALES
! -----------------
    integer :: ic
    real(kind=8) :: dnorm, dnorm0, temp, tolch, tole, zero
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  ABS
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    ivar = 0
    tolch = 10.0d0 * toln
    zero = 0.0d0
!
    if (dt0 .eq. zero) goto 999
!
!---- BOUCLE SUR LES NON-LINEARITES
!
    do 10 ic = 1, nbnl
!
!------- CHOIX DE LA TOLERANCE EN FONCTION DE LA POSITION DU NOEUD DE
!------- CHOC PAR RAPPORT A LA BUTEE
!
        dnorm0 = tconf1(4,ic)
        if (abs(dnorm0) .lt. tolch) dnorm0 = zero
        dnorm = tconf2(4,ic)
        if (abs(dnorm ) .lt. tolch) dnorm = zero
!
!....... CHOC A L'INSTANT N+1
        if (dnorm .lt. zero) then
!.......... CHOC OU CONTACT EXACT A L'INSTANT N
            if (dnorm0 .le. zero) then
                tole = tolc
!.......... VOL A L'INSTANT N
            else
                tole = tolv
            endif
!
!....... CONTACT EXACT A L'INSTANT N+1
        else if (dnorm.eq.zero) then
!.......... CHOC A L'INSTANT N
            if (dnorm0 .lt. zero) then
                tole = tolc
!.......... CONTACT EXACT A L'INSTANT N
            else if (dnorm0.eq.zero) then
                tole = tolc
!.......... VOL A L'INSTANT N
            else
                tole = tolv
            endif
!
!....... VOL A L'INSTANT N+1
        else
!.......... CHOC A L'INSTANT N
            if (dnorm0 .lt. zero) then
                tole = tolc
!.......... CONTACT EXACT OU VOL A L'INSTANT N
            else
                tole = tolv
            endif
        endif
!
!------- TEST SUR LA PREMIERE COMPOSANTE DU DEPLACEMENT (REPERE LOCAL)
!------- POUR LA BUTEE IC
!
        if (dep(1,ic) .ne. zero) then
            temp = abs(dep(1,ic) - dep0(1,ic))
            if (temp .gt. tole) then
                ivar = 1
                goto 999
            endif
        endif
!
!------- TEST SUR LA DEUXIEME COMPOSANTE DU DEPLACEMENT (REPERE LOCAL)
!------- POUR LA BUTEE IC
!
        if (dep(2,ic) .ne. zero) then
            temp = abs(dep(2,ic) - dep0(2,ic))
            if (temp .gt. tole) then
                ivar = 1
                goto 999
            endif
        endif
!
!------- TEST SUR LA TROISIEME COMPOSANTE DU DEPLACEMENT (REPERE LOCAL)
!------- POUR LA BUTEE IC
!
        if (dep(3,ic) .ne. zero) then
            temp = abs(dep(3,ic) - dep0(3,ic))
            if (temp .gt. tole) then
                ivar = 1
                goto 999
            endif
        endif
!
10  end do
!
999  continue
!
! --- FIN DE VARDEP.
end subroutine
