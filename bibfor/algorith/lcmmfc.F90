subroutine lcmmfc(coeft, ifa, nmat, nbcomm, necrci,&
                  itmax, toler, alpham, dgamma, dalpha,&
                  iret)
    implicit none
! person_in_charge: jean-michel.proix at edf.fr
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
    include 'asterc/r8miem.h'
    include 'asterfort/lcine2.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/zeroco.h'
    integer :: nmat, ifa, nbcomm(nmat, 3), iret, itmax
    real(kind=8) :: coeft(nmat), dgamma, dalpha, toler
    character(len=16) :: necrci
! ======================================================================
!  INTEGRATION DES LOIS MONOCRISTALLINES : ECROUISSAGE CINEMATIQUE
! ======================================================================
!       IN  COEFT   :  PARAMETRES MATERIAU
!           IFA     :  NUMERO DE FAMILLE
!           NBCOMM  :  NOMBRE DE COEF MATERIAU PAR FAMILLE
!           NECRCI  :  NOM DE LA LOI D'ECROUISSAGE CINEMATIQUE
!           DGAMMA  :  DERIVEES DES VARIABLES INTERNES A T
!           ALPHAM  : VARIABLE ECRO CINE A T
!           ITMAX  :  ITER_INTE_MAXI
!           TOLER  :  RESI_INTE_RELA
!     OUT:
!           DALPHA  : VARIABLE INTERNE ECROUISSAGE CINEMATIQUE
!           IRET    : CODE RETOUR
!
!     ----------------------------------------------------------------
    real(kind=8) :: d, gm, pm, c, cc, alpham, absdga, x(4), y(4)
    real(kind=8) :: f0, x1, fmax
    integer :: iec, iter, nuecin
!     ----------------------------------------------------------------
!
!     DANS VIS : 1 = ALPHA, 2=GAMMA, 3=P
!
    iec=nbcomm(ifa,2)
    absdga=abs(dgamma)
    nuecin=nint(coeft(iec))
!
!----------------------------------------------------------------------
!   POUR UN NOUVEAU TYPE D'ECROUISSAGE CINEMATIQUE, AJOUTER UN BLOC IF
!----------------------------------------------------------------------
!
    iret=0
!      IF (NECRCI.EQ.'ECRO_CINE1') THEN
    if (nuecin .eq. 1) then
!          D=COEFT(IEC-1+1)
        d=coeft(iec+1)
        dalpha=(dgamma-d*alpham*absdga)/(1.0d0+d*absdga)
!
!      IF (NECRCI.EQ.'ECRO_CINE2') THEN
    else if (nuecin.eq.2) then
        iret=0
!           D=COEFT(IEC-1+1)
!           GM=COEFT(IEC-1+2)
!           PM=COEFT(IEC-1+3)
!           C=COEFT(IEC-1+4)
        d =coeft(iec+1)
        gm=coeft(iec+2)
        pm=coeft(iec+3)
        c =coeft(iec+4)
        cc=c*alpham
        if (cc .eq. 0.d0) then
            dalpha=(dgamma-d*alpham*absdga)/(1.0d0+d*absdga)
        else
!            RECHERCHE DE DALPHA PAR SECANTE. dF/dAlpha TOUJOURS >0
            f0=lcine2(d,gm,pm,c,dgamma,alpham,0.d0)
            if (abs(f0) .le. toler) then
                dalpha = 0.d0
                goto 50
            else if (f0.le.0.d0) then
                x(1) = 0.d0
                y(1) = f0
!               F0 < 0 , ON CHERCHE X TEL QUE FMAX > 0 :
                x1 =(dgamma-d*alpham*absdga)/(1.0d0+d*absdga)
                if (abs(x1) .le. r8miem()) x1=1.d-10
                do 10 iter = 1, itmax
                    fmax=lcine2(d,gm,pm,c,dgamma,alpham,x1)
                    if (fmax .ge. 0.d0) then
                        x(2) = x1
                        y(2) = fmax
                        goto 20
                    else
                        x1 = x1*2.d0
                    endif
10              continue
                goto 60
            else
                x(2) = 0.d0
                y(2) = f0
!               F0 > 0 , ON CHERCHE X TEL QUE FMAX < 0 :
                x1 =(dgamma-d*alpham*absdga)/(1.0d0+d*absdga)
                if (abs(x1) .le. r8miem()) x1=-1.d-10
                do 30 iter = 1, itmax
                    fmax=lcine2(d,gm,pm,c,dgamma,alpham,x1)
                    if (fmax .le. 0.d0) then
                        x(1) = x1
                        y(1) = fmax
                        goto 20
                    else
                        x1 = x1*2.d0
                    endif
30              continue
                goto 60
            endif
20          continue
!             CALCUL DE X(4) SOLUTION DE L'EQUATION F = 0 :
            x(3) = x(1)
            y(3) = y(1)
            x(4) = x(2)
            y(4) = y(2)
            do 40 iter = 1, itmax
                if (abs(y(4)) .lt. toler) goto 50
                call zeroco(x, y)
                dalpha = x(4)
                y(4)=lcine2(d,gm,pm,c,dgamma,alpham,dalpha)
40          continue
60          continue
!               CALL INFNIV(IFM,NIV)
!               WRITE (IFM,*) 'ECRO_CIN2 : NON CONVERGENCE'
!               WRITE (IFM,*) 'VALEURS DE X ET Y ',X,Y
            iret = 1
50          continue
        endif
    else
        call u2mess('F', 'COMPOR1_19')
    endif
!
end subroutine
