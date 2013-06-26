subroutine cmphii(ck, cm, ndim, nbmod, niter,&
                  xcrit, ceigen, cmod, ndimax, cmat1,&
                  cmat2, cvec, ific)
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
! aslint: disable=W1306
    implicit none
!
!***********************************************************************
!    P. RICHARD                                 DATE 31/07/91
!-----------------------------------------------------------------------
!  BUT:  < COMPLEXE MODES PROBLEME HERMITIEN ITERATION INVERSE >
!
!   CALCULER LES PREMIERS MODES PROPRES D'UN PROBLEME
!       AUX VALEURS PROPRES A MATRICES RAIDEURS ET MASSE COMPLEXES
!       HERMITTIENNES STOCKEES TRIANGULAIRES SUPERIEURES
!
!                     CK*X= L CM*X
!
!    METHODE D'ITERATION INVERSE
!
!-----------------------------------------------------------------------
!
! CK       /I/: MATRICE RAIDEUR DU PROBLEME
! CM       /I/: MATRICE MASSE DU PROBLEME
! NDIM     /I/: DIMENSION DES MATRICES
! NBMOD    /I/: NOMBRE DE MODES PROPRES DESIRE
! NITER    /I/: NOMBRE MAX D'ITERATIONS PAR MODE
! XCRIT    /I/: TOLERANCE DE COLINEARITE RELATIVE (CRITERE CONVERGENCE)
! CEIGEN   /O/: VALEURS PROPRES COMPLEXES DU PROBLEME
! CMOD     /O/: MODES PROPRES COMPLEXES SOLUTIONS
! NDIMAX   /I/: NOMBRE DE DDL GENERALISES DES MODES >=NDIM
! CMAT1    /M/: MATRICE COMPLEXE DE TRAVAIL
! CMAT2    /M/: MATRICE COMPLEXE DE TRAVAIL
! CVEC     /M/: VECTEUR COMPLEXE DE TRAVAIL
! IFIC     /I/: NUMERO UNITE LOGIQUE POUR MESSAGE
!
!-----------------------------------------------------------------------
!
    include 'asterfort/cmatve.h'
    include 'asterfort/cschmi.h'
    include 'asterfort/ctescv.h'
    include 'asterfort/cvalea.h'
    include 'asterfort/rrldc.h'
    include 'asterfort/sesqui.h'
    include 'asterfort/trldc.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'blas/zcopy.h'
    integer :: vali(2)
    complex(kind=8) :: ck(*), cm(*), ceigen(nbmod)
    complex(kind=8) :: cmod(ndimax, nbmod), cprod, cmod0(ndim)
    complex(kind=8) :: cmat1(*), cmat2(ndim, ndim), cvec(ndim), cvec0(ndim)
    logical :: convok
    integer :: i, idiag, ific, ipivo, iv, ivdiag, j
    integer :: k, nbmod, ndim, ndimax, niter
    real(kind=8) :: valr(3), xcrit, xer
    character(len=6) :: valk
!-----------------------------------------------------------------------
!
    valk = 'CMPHII'
    call u2mesk('I', 'ALGELINE7_2', 1, valk)
    call u2mess('I', 'ALGELINE7_3')
!
!      RECOPIE DE LA MATRICE DE RAIDEUR
    call zcopy(ndim*(ndim+1)/2, ck, 1, cmat1, 1)
!
!    FACTORISATION DE LA MATRICE DE RAIDEUR
    call trldc(cmat1, ndim, ipivo)
!    GESTION DU PIVOT NUL
    if (ipivo .ne. 0) then
        vali(1) = ipivo
        call u2mesi('F', 'ALGORITH12_53', 1, vali)
    endif
!
!
!   CALCUL DE L'INVERSE DE LA MATRICE DE MASSE
    do 40 iv = 1, ndim
        ivdiag = iv*(iv-1)/2+1
        do 20 i = 1, ndim
            if (i .le. iv) then
                cmat2(i,iv)=cm(ivdiag+iv-i)
            else
                idiag = i*(i-1)/2+1
                cmat2(i,iv)=dconjg(cm(idiag+i-iv))
            endif
20      continue
40  end do
    call rrldc(cmat1, ndim, cmat2, ndim)
!
    do 66 iv = 1, ndim
        cvec(iv)=dcmplx(0.d0,0.d0)
        cvec0(iv)=dcmplx(0.d0,0.d0)
        cmod0(iv)=dcmplx(0.d0,0.d0)
66  end do
!
!
!   INITIALISATION ALEATOIRE DES VECTEURS PROPRES DE DEPART
    call cvalea(ndim, cmod, ndimax, nbmod)
!
!
!     DEBUT DE LA BOUCLE D'ITERATION SUR LES MODES
!
    do 50 j = 1, nbmod
!
!       INITIALISATION DES CRITERES D'ARRET
        k=0
        convok=.true.
!
!   BOUCLE D'ITERATION SUR CHAQUE MODES
100      continue
!
        k=k+1
!
!    PRODUIT MATRICIEL INV(M)*VECTEUR
        call cmatve(cmat2, cmod(1, j), cvec, ndim)
!
!    CALCUL DE L'ERREUR COLINEARITE ET REECOPIE
!    DE CVEC DANS CMOD
        call ctescv(cvec, cmod(1, j), cvec0, cmod0, ndim,&
                    xer)
!
!      RECOPIE DU VECTEUR DE L'ITERATION PRECEDENTE
        call zcopy(ndim, cmod(1, j), 1, cmod0, 1)
        call zcopy(ndim, cvec, 1, cvec0, 1)
!
!   ORTHORMALISATION PAR RAPPORT MATRICE DE MASSE
        call cschmi(cm, ndim, cmod(1, j), cmod, ndimax,&
                    j-1)
!
!
!   CALCUL VALEURS PROPRES PAR COEF RAYLEIGH
!    En fait on calcul explicitement CMOD*inv(M)*K*CMOD
        call sesqui(ck, cmod(1, j), ndim, ceigen(j))
        call sesqui(cm, cmod(1, j), ndim, cprod)
        ceigen(j)=ceigen(j)/cprod
!
!         TEST SUR LA PRECISION
        if (xer .le. xcrit) convok=.false.
!
        if (k .lt. niter .and. convok) goto 100
!
!
!     IMPRESSION DES FREQUENCES PROPRES
        vali(1)=j
        vali(2)=k
        valr(1)=xer
        valr(2)=dble(ceigen(j))
        valr(3)=dimag(ceigen(j))
        call u2mesg('I', 'ALGELINE7_4', 0, ' ', 2,&
                    vali, 3, valr)
!
50  end do
!
    write(ific,*)'     '
    write(ific,*)'     '
!
end subroutine
