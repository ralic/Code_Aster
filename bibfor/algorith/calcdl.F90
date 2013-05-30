subroutine calcdl(vp, i1e, sigeqe, nbmat, materf,&
                  parame, derive, sig3, vecp, eta,&
                  dg, se, detadg, dgdl, ddlde)
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
! =====================================================================
    implicit none
    include 'asterfort/lcinma.h'
    integer :: nbmat
    real(kind=8) :: ddlde(6), vecp(3, 3), materf(nbmat, 2), parame(4), derive(5)
    real(kind=8) :: vp(3), i1e, sigeqe, eta, se(6), dg, detadg, sig3, dgdl
! ======================================================================
! --- LOI DE HOEK BROWN : CALCUL DE DDLAMBDA/DEPS ---------------------
! ======================================================================
! IN  SE      DEVIATEUR ELASTIQUE --------------------------------------
! IN  VP      VALEURS PROPRES DU DEVIATEUR ELASTIQUE SE ----------------
! IN  I1E     TRACE DE SE ----------------------------------------------
! IN  NBMAT   NOMBRE DE DONNEES MATERIAU -------------------------------
! IN  MATERF  DONNEES MATERIAU -----------------------------------------
! IN  PARAME  VALEUR DES PARAMETRES DE LA LOI S*SIG, M*SIG, B ----------
! IN  DERIVE  VALEUR DES DERIVEES DES PARAMETRES PAR RAPPORT A GAMMA ---
! IN  SIG3    CONTRAINTE PRINCIPALE SIG3 -------------------------------
! IN  VECP    VECTEURS PROPRES DE SE -----------------------------------
! IN  DG      INCREMENT DU PARAMETRE D ECROUISSAGE GAMMA ---------------
! IN  DETADG  DERIVEE DE ETA PAR RAPPORT A GAMMA -----------------------
! IN  DGDL    DERIVEE  DE GAMMA PAR RAPPORT A LAMBDA -------------------
! OUT DDLDE   DDLAMDA/DEPS ---------------------------------------------
! ======================================================================
    real(kind=8) :: dsdde(6, 6), un, deux, trois, k, dl, mu
    real(kind=8) :: da1de(6), da2de(6), da3de(6), da6de(6)
    real(kind=8) :: a2, a3, a4, c5, a6, aux1, aux2, aux3, denom, aux4
    integer :: ii, ndt, ndi, jj
! =================================================================
    parameter       ( un     =  1.0d0  )
    parameter       ( deux   =  2.0d0  )
    parameter       ( trois  =  3.0d0  )
! ======================================================================
    common /tdim/   ndt, ndi
! ======================================================================
! --- INITIALISATIONS --------------------------------------------------
! ======================================================================
    call lcinma(0.0d0, dsdde)
    mu = materf(4,1)
    k = materf(5,1)
! ======================================================================
    a2 = vp(3)-vp(1)
    a3 = trois*mu/sigeqe
    a4 = trois*k*eta
    c5 = un/materf(14,2)
    a6 = a3*vp(3)
    dl = dg/(eta+un)
! ======================================================================
! --- CALCUL DE DSD/DE ------------------------------------------------
! ======================================================================
    do 40 ii = 1, ndi
        do 50 jj = 1, ndi
            dsdde(ii,jj) = deux*mu*vecp(ii,jj)*vecp(ii,jj) -deux*mu*( vecp(1,ii)**2+vecp(2,ii)**2&
                           &+vecp(3,ii)**2)/trois
50      continue
        dsdde(ii,4) = deux*mu*vecp(ii,1)*vecp(ii,2) -deux*mu*(vecp(1, 1)*vecp(1,2)+vecp(2,1)*vecp&
                      &(2,2) +vecp(3,1)*vecp(3,2))/trois
        if (ndt .eq. 6) then
            dsdde(ii,5) = deux*mu*vecp(ii,1)*vecp(ii,3) -deux*mu*( vecp(1,1)*vecp(1,3)+vecp(2,1)*&
                          &vecp(2,3) +vecp(3,1)*vecp(3, 3))/trois
            dsdde(ii,6) = deux*mu*vecp(ii,2)*vecp(ii,3) -deux*mu*( vecp(1,3)*vecp(1,2)+vecp(2,3)*&
                          &vecp(2,2) +vecp(3,3)*vecp(3, 2))/trois
        endif
40  end do
    do 55 jj = 1, ndi
        dsdde(4,jj) = deux*mu*vecp(1,jj)*vecp(2,jj)
55  end do
    dsdde(4,4) = deux*mu*vecp(1,1)*vecp(2,2)
    if (ndt .eq. 6) then
        do 60 jj = 1, ndi
            dsdde(5,jj) = deux*mu*vecp(1,jj)*vecp(3,jj)
            dsdde(6,jj) = deux*mu*vecp(2,jj)*vecp(3,jj)
60      continue
        dsdde(4,5) = deux*mu*vecp(1,1)*vecp(2,3)
        dsdde(4,6) = deux*mu*vecp(1,2)*vecp(2,3)
        dsdde(5,4) = deux*mu*vecp(1,1)*vecp(3,2)
        dsdde(5,5) = deux*mu*vecp(1,1)*vecp(3,3)
        dsdde(5,6) = deux*mu*vecp(1,2)*vecp(3,3)
        dsdde(6,4) = deux*mu*vecp(2,1)*vecp(3,2)
        dsdde(6,5) = deux*mu*vecp(2,1)*vecp(3,3)
        dsdde(6,6) = deux*mu*vecp(2,2)*vecp(3,3)
    endif
! ====================================================================
! --- ON TRAITE LE CAS DE DEUX VALEURS PROPRES EGALES ----------------
! ====================================================================
    if ((abs(vp(3)-vp(2)).lt.1.d-8) .or. (abs(vp(3)-vp(2)).lt.(max(vp(3),vp(2))*1.d-8))) then
        do 153 ii = 1, 3
            aux1 = dsdde(ii,2)+dsdde(ii,3)
            dsdde(ii,2) = 0.5d0*aux1
            dsdde(ii,3) = 0.5d0*aux1
153      continue
    endif
    if ((abs(vp(1)-vp(2)).lt.1.d-8) .or. (abs(vp(1)-vp(2)).lt.(max(vp(1),vp(2))*1.d-8))) then
        do 154 ii = 1, 3
            aux1 = dsdde(ii,2)+dsdde(ii,1)
            dsdde(ii,2) = 0.5d0*aux1
            dsdde(ii,1) = 0.5d0*aux1
154      continue
    endif
! =====================================================================
    do 70 ii = 1, ndt
        da1de(ii) = dsdde(ii,3)
70  end do
    do 80 ii = 1, ndi
        da1de(ii) = da1de(ii) + k
80  end do
    do 75 ii = 1, ndt
        da2de(ii) = dsdde(ii,3)-dsdde(ii,1)
75  end do
    do 85 ii = 1, ndt
        da3de(ii) = -9.0d0*mu*mu*se(ii)/(sigeqe**3)
85  end do
    do 95 ii = 1, ndt
        da6de(ii) = vp(3)*da3de(ii)+a3*dsdde(ii,3)
95  end do
! ======================================================================
! --- CALCUL DU DENOMINATEUR -------------------------------------------
! ======================================================================
    aux1 = parame(1)-parame(2)*sig3
    aux2 = dl*trois*k*detadg*dgdl+a6+a4
    aux3 = dgdl*(derive(1) -sig3*derive(2)) + parame(2)*aux2
    denom = -a2*a3 -derive(3)*dgdl*(un+c5*sig3)+parame(3)*c5*aux2 - aux3/(sqrt(aux1)*deux)
! ======================================================================
! --- CALCUL DE DDL/DEPS -----------------------------------------------
! ======================================================================
    do 100 ii = 1, ndi
        aux4 = da1de(ii)-da6de(ii)*dl
        ddlde(ii) = (&
                    -(un-a3*dl)*da2de(ii)+a2*da3de(ii)*dl +parame(3)* c5*aux4 -parame(2)*aux4/(de&
                    &ux*sqrt(aux1))&
                    )/denom
100  end do
    do 102 ii = ndi+1, ndt
        aux4 = da1de(ii)-da6de(ii)*dl
        ddlde(ii) = (&
                    -(un-a3*dl)*da2de(ii) +a2*da3de(ii)*dl+parame(3)* c5*aux4 -parame(2)*aux4/(de&
                    &ux*sqrt(aux1))&
                    )/denom
102  end do
    do 110 ii = ndt+1, 6
        ddlde(ii) = 0.0d0
110  end do
! ======================================================================
end subroutine
