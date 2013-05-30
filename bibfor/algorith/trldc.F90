subroutine trldc(a, nordre, ierr)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!***********************************************************************
    implicit none
!
!    A. COMTE                                 DATE 31/07/91
!-----------------------------------------------------------------------
!  BUT: FACTORISATION LDLT EN PLACE D'UNE MATRICE COMPLEXE
!  ET HERMITIENNE STOCKEE TRIANGULAIRE SUPERIEURE
!
!  CODE RETOUR  SI =0 RAS
!               SI NON NUL ALORS EST EGAL AU RANG DU PIVOT NUL TROUVE
!
!-----------------------------------------------------------------------
!
! A        /M/: MATRICE COMPLEXE A FACTORISER
! NORDRE   /I/: DIMENSION DE LA MATRICE
! IERR     /O/: CODE RETOUR
!
!-----------------------------------------------------------------------
!
    include 'asterc/r8gaem.h'
    include 'asterfort/u2mess.h'
    complex(kind=8) :: a(*), r8val
    real(kind=8) :: epsi, xmod, xmax, zero
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ibm, idiag, ierr, in, indiag, jn
    integer :: jndiag, nordre
!-----------------------------------------------------------------------
    data zero /0.d+00/
!-----------------------------------------------------------------------
!
    epsi=1.d0/r8gaem()
    xmax=zero
    ierr = 0
    do 100 in = 1, nordre
        indiag = in*(in-1)/2+1
        if (in .eq. 1) goto 50
!
!        UTILISATION  DES  LIGNES  (1) A (IN-1)
        do 40 jn = 1, in-1
            jndiag = jn*(jn-1)/2+1
!
            if (jn .eq. 1) goto 36
            ibm = jn - 1
!
            r8val = a (indiag+in-jn)
            do 30 i = 1, ibm
                idiag = i*(i-1)/2+1
                r8val = r8val - dconjg(a(jndiag+jn-i))*a(indiag+in-i)* a(idiag)
30          continue
            a ( indiag+in-jn ) = r8val
!
36          continue
            a (indiag+in-jn ) = a (indiag+in-jn ) / a(jndiag)
40      continue
!
50      continue
!
!        UTILISATION  DE LA LIGNE IN ( CALCUL DU TERME PIVOT)
        ibm = in - 1
!
        r8val = a (indiag)
        do 85 i = 1, ibm
            idiag = i*(i-1)/2+1
            r8val = r8val - dconjg(a(indiag+in-i))*a(indiag+in-i)*a( idiag)
85      continue
        a (indiag) = r8val
        xmod=dble(r8val)**2+dimag(r8val)**2
        if (xmod .gt. xmax) xmax=xmod
        if ((xmod/xmax) .lt. epsi) then
            call u2mess('I', 'ALGORITH10_98')
            ierr = in
            goto 9999
        endif
!
100  end do
!
9999  continue
end subroutine
