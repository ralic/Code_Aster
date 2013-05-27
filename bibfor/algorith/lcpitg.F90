subroutine lcpitg(compor, df, line, dp, dvbe,&
                  dtaudf)
!
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
!
    implicit none
!
! ----------------------------------------------------------------------
!        INTEGRATION DE LA LOI SIMO MIEHE ECROUISSAGE ISOTROPE
!  DERIVEE DE TAU PAR RAPPORT A DF * DFT = TAUDF(IJ,K,P)*DF(L,P)
!  TAU = TAU(DVTAU - TRTAU)
!  TRTAU = TRTAU(DF)
!  DVTAU=DVTAU(DVBE)
!  DVBE = DVBE(BETR)
!  BETR =  ETR(DFB)
!  DFB=DVB(DF)
! ----------------------------------------------------------------------
! IN  COMPOR : COMPORTEMENT
! IN  DF     : INCREMENT DU TENSEUR DE DEFORMATION
! IN  LINE : REGIME DE LA SOLUTION (ELASTIQUE, PLASTIQUE)
! IN  DP     : INCREMENT DE DEFORMATION PLASTIQUE
! IN  DVBE   : PARTIE DEVIATORIQUE DE LA DEFORMATION
! OUT DTAUDF : DERIVEE DE TAU PAR RAPPORT A DF * DFT
! ----------------------------------------------------------------------
    include 'blas/ddot.h'
    character(len=16) :: compor
    integer :: line
    real(kind=8) :: df(3, 3), dp, dvbe(6), dtaudf(6, 3, 3)
!
! COMMON GRANDES DEFORMATIONS SIMO - MIEHE
!
    integer :: ind(3, 3), ind1(6), ind2(6)
    real(kind=8) :: kr(6), rac2, rc(6), id(6, 6)
    real(kind=8) :: bem(6), betr(6), dvbetr(6), eqbetr, trbetr
    real(kind=8) :: jp, dj, jm, dfb(3, 3)
    real(kind=8) :: djdf(3, 3), dbtrdf(6, 3, 3)
!
    common /gdsmc/&
     &            bem,betr,dvbetr,eqbetr,trbetr,&
     &            jp,dj,jm,dfb,&
     &            djdf,dbtrdf,&
     &            kr,id,rac2,rc,ind,ind1,ind2
! ----------------------------------------------------------------------
!  COMMON MATERIAU POUR VON MISES
!
    integer :: jprol, jvale, nbval
    real(kind=8) :: pm, young, nu, mu, unk, troisk, cother
    real(kind=8) :: sigm0, epsi0, dt, coefm, rpm, pente, apui, npui, sigy
!
    common /lcpim/&
     &          pm,young,nu,mu,unk,troisk,cother,&
     &          sigm0,epsi0,dt,coefm,rpm,pente,&
     &          apui,npui,sigy,jprol,jvale,nbval
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: ij, kl, k, l
    real(kind=8) :: a2, a3, a4, rb, arg, eqbe
    real(kind=8) :: dtaudj, dtaudb
    real(kind=8) :: dvbbtr(6, 6), dvbedf(6, 3, 3)
! ----------------------------------------------------------------------
!
! 1 - DEFINITION DES COEFFICIENTS UTILES
!
!
    if (line .eq. 0) then
        a2 = 1
        a3 = 0
        a4 = 0
    else
        eqbe=sqrt(1.5d0*ddot(6,dvbe,1,dvbe,1))
        a2=1/(1+trbetr*dp/eqbe)
!
        rb=pente
        if (compor(1:4) .eq. 'VISC' .and. dp .ne. 0.d0) then
            arg = (dp/(dt*epsi0))**(1.d0/coefm)
            rb = rb + sigm0*arg/(sqrt(arg**2+1.d0)*coefm*dp)
        endif
!
        a3=(a2*dp/eqbe)-mu/(rb+mu*trbetr)
        a3=3.d0*trbetr*a3/(2.d0*eqbe*eqbe)
!
        a4=dp/eqbe*((mu*trbetr/(rb+mu*trbetr))-1.d0)
    endif
!
!
! 2 - DERIVEE DE DVBE PAR RAPPORT A BETR = DVBBTR
!
    do 110 ij = 1, 6
        do 120 kl = 1, 6
            dvbbtr(ij,kl)= a2*(id(ij,kl)-kr(ij)*kr(kl)/3.d0) + a3*&
            dvbe(ij)*dvbe(kl) + a4*dvbe(ij)*kr(kl)
120      continue
110  end do
!
!
! 3 - DERIVEE DE DVBE PAR RAPPORT A DF = DVBEDF
!
    do 130 ij = 1, 6
        do 140 k = 1, 3
            do 150 l = 1, 3
                dvbedf(ij,k,l) = ddot(6,dvbbtr(ij,1),6,dbtrdf(1,k,l), 1)
150          continue
140      continue
130  end do
!
!
! 4 - MATRICE TANGENTE = DTAUDF
!
!    DERIVEE PARTIELLE DE TAU PAR RAPPORT A B ET J
    dtaudb = mu
    dtaudj = 0.5d0*(2.d0*unk*jp-cother*(1.d0-1.d0/(jp**2.d0)))
!
    do 170 ij = 1, 6
        do 180 k = 1, 3
            do 190 l = 1, 3
                dtaudf(ij,k,l) = dtaudb*dvbedf(ij,k,l) + dtaudj*kr(ij) *djdf(k,l)
190          continue
180      continue
170  end do
!
end subroutine
