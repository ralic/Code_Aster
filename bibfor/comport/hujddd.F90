subroutine hujddd(carac, k, mater, ind, yf,&
                  vin, vec, mat, iret)
    implicit none
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
! ---------------------------------------------------------------------
! CALCUL DE DIFFERENTES DERIVEES POUR LE MECANISME K
! ---------------------------------------------------------------------
!  IN
!   CARAC  :  = 'DFDS'   DERIVE PREMIERE DU SEUIL F PAR RAPPORT A SIGMA
!             = 'PSI'
!             = 'DPSIDS' DERIVE DE LA LOI D'ECOULEMENT (K=1,2,3)
!                        PAR RAPPORT A SIGMA
!   K      :  NUMERO DU MECANISME (1 A 8)
!   MATER  :  PARAMETRES MATERIAU
!   IND    :  TABLEAU DE CORRESPONDANCE
!             NUMERO D'ORDRE / NUMERO DE MECANISME
!   YF     :  VECTEUR DES INCONNUES
!   VIN    :  VARIABLES INTERNES A T
!  OUT
!   VEC    :  VECTEUR SOLUTION PSI OU DFDS
!   MAT    :  MATRICE SOLUTION DPSIDS
!   IRET   :  CODE RETOUR
!                  = 0   OK
!                  = 1   NOOK
! =====================================================================
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/hujksi.h"
#include "asterfort/infniv.h"
#include "asterfort/lcinma.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
    integer :: ndt, ndi, i, j, k, mod, kk, nbmect
    integer :: ind(7), nbmeca, iret, iadzi, iazk24
    integer :: ifm, niv
    parameter     (mod = 18)
    real(kind=8) :: beta, m, pco, pcoh, pc, pref, alpha
    real(kind=8) :: epsvpd, b, phi, angdil
    real(kind=8) :: dd, p, q, si, sigkij
    real(kind=8) :: yf(mod), r, sigf(6), sigd(6)
    real(kind=8) :: mater(22, 2), vec(6), mat(6, 6), ksi
    real(kind=8) :: d12, d13, un, zero, deux
    real(kind=8) :: tole1, degr, aexp, exptol
    real(kind=8) :: xk(2), th(2), sigdc(6), qc, vin(*), d14, d40
    real(kind=8) :: dsdds(6, 6), sxs(6, 6), sxp(6, 6), pxp(6, 6)
    real(kind=8) :: vp(6), prod, scxp(6, 6), ps, pxh(6, 6)
    real(kind=8) :: vhist(6), sxh, scxh, fac, ptrac
    character(len=6) :: carac
    character(len=8) :: nomail
    aster_logical :: consol, tract, debug, dila
! =====================================================================
    parameter     ( d12   = 0.5d0  )
    parameter     ( d14   = 0.25d0 )
    parameter     ( d13   = 0.3333333333334d0 )
    parameter     ( un    = 1.d0   )
    parameter     ( zero  = 0.d0   )
    parameter     ( deux  = 2.d0   )
    parameter     ( tole1 = 1.d-7  )
    parameter     ( degr  = 0.0174532925199d0 )
    parameter     ( d40   = 40.0d0 )
! =====================================================================
    common /tdim/   ndt, ndi
    common /meshuj/ debug
!
    call infniv(ifm, niv)
!
!
! =====================================================================
! --- PROPRIETES HUJEUX MATERIAU --------------------------------------
! =====================================================================
    beta = mater(2,2)
    b = mater(4,2)
    phi = mater(5,2)
    angdil = mater(6,2)
    pco = mater(7,2)
    pref = mater(8,2)
    m = sin(degr*phi)
    alpha = mater(20,2)*d12
    ptrac = mater(21,2)
!
!
! =====================================================================
! --- PREMIER INVARIANT ET AUTRES GRANDEURS UTILES --------------------
! =====================================================================
    nbmeca = 0
    nbmect = 0
    do 4 i = 1, 7
        if (ind(i) .gt. 0) then
            nbmect = nbmect+1
            if (ind(i) .le. 8) nbmeca = nbmeca + 1
        endif
  4 end do
!
    do 5 i = 1, nbmeca
        if (ind(i) .eq. k) r = yf(ndt+1+i)
  5 continue
!
    epsvpd = yf(ndt+1)
!
    exptol = log(r8maem())
    exptol = min(exptol, d40)
    aexp = -beta*epsvpd
!
    if (aexp .ge. exptol) then
        iret = 1
        goto 999
    endif
!
    pc = pco*exp(-beta*epsvpd)
    if ((pc/pref) .lt. tole1) then
        iret = 1
        goto 999
    endif
!
    do 25 i = 1, ndt
        sigf(i) = yf(i)
 25 continue
!
!
! ---> PROJECTION DES CONTRAINTES DANS LE PLAN DEVIATEUR K
!        CALCUL DU DEVIATIEUR SIGDK (6X1), PK ET QK
!        CALL HUJPROJ ( K, SIGF, SIGK, SIGDK, PK, QK )
!af 09/05/07 Debut
    if (k .eq. 4 .or. k .ge. 8) goto 100
!af 09/05/07 Fin
!
    do 27 i = 1, 6
        sigd(i) = zero
 27 continue
!
    dd = zero
    p = zero
    si = un
    do 10 i = 1, ndi
        if (k .lt. 4) then
            if (i .ne. k) then
                p = p + sigf(i)
                dd = dd + sigf(i)*si
                si = -si
            endif
        else
            if (i .ne. (k-4)) then
                p = p + sigf(i)
                dd = dd + sigf(i)*si
                si = -si
            endif
        endif
 10 continue
    dd = d12*dd
    p = d12*p
!
!kham --- ON ENTRE DANS LE DOMAINE "COHESIF"
    if ((p/pref) .lt. tole1) then
        dila = .true.
        pcoh = pref*tole1
    else
        dila = .false.
        pcoh = p
    endif
!kh ---
!
    si = un
    do 11 i = 1, ndi
        if (k .lt. 4) then
            if (i .ne. k) then
                sigd(i) = dd*si
                si = -si
            endif
        else
            if (i .ne. (k-4)) then
                sigd(i) = dd*si
                si = -si
            endif
        endif
 11 continue
    if (k .lt. 4) then
        sigkij = sigf( ndt+1-k )
        sigd( ndt+1-k ) = sigkij
    else
        sigkij = sigf( ndt+5-k )
        sigd( ndt+5-k ) = sigkij
    endif
!af 14/05/07 Debut
!
    if (k .lt. 4) then
        q = dd**deux + (sigkij**deux)/deux
        q = sqrt(q)
        consol = (-q/pref).le.tole1
    endif
!
    tract = ((p -ptrac)/pref) .lt. tole1
    if (tract) then
        if (debug) then
            call tecael(iadzi, iazk24)
            nomail=zk24(iazk24-1+3) (1:8)
            write(6,'(10(A))') 'HUJDDD :: TRACTION DANS LA MAILLE ',&
            nomail
            write(6,'(A,6(1X,E16.9))')'P =',p
        endif
        goto 100
    endif
!af 14/05/07 Fin
!af 09/05/07 Debut
!
!
! ====================================================================
! --- CALCUL DE SIGDC ET QC POUR MECANISME DEVIATOIRE CYCLIQUE -------
! ====================================================================
    if ((k.gt.4) .and. (k.lt.8)) then
        do 29 i = 1, ndt
            sigdc(i)=zero
 29     continue
!
        xk(1) = vin(4*k-11)
        xk(2) = vin(4*k-10)
        th(1) = vin(4*k-9)
        th(2) = vin(4*k-8)
        si=un
        do 30 i = 1, 3
            if (i .ne. (k-4)) then
                sigdc(i) = sigd(i)-(xk(1)-r*th(1))*(p -ptrac)*si* (un-b*log((p -ptrac)/pc))*m
                si = -si
            endif
 30     continue
!
        sigdc(ndt+5-k) = sigd(ndt+5-k)-(xk(2)-r*th(2))*(p -ptrac)* (un-b*log((p -ptrac)/pc))*m
        qc = sqrt(&
             d12*( sigdc(1)**2+sigdc(2)**2+ sigdc(3)**2+sigdc(4)** 2+sigdc(5)**2+sigdc(6)**2 ))
        consol = (-qc/pref).le.tole1
    endif
100 continue
!
!
! ====================================================================
! --- CALCUL DE PCK POUR MECANISME SPHERIQUE CYCLIQUE ----------------
! ====================================================================
    if (k .eq. 8) then
        exptol = log(r8maem())
        exptol = min(exptol, d40)
        aexp = -beta*epsvpd
        if (aexp .ge. exptol) then
            call utmess('F', 'COMPOR1_7')
        endif
        p = (sigf(1)+sigf(2)+sigf(3))*d13
    endif
    if (k .eq. 4) p = (sigf(1)+sigf(2)+sigf(3))*d13
!af 09/05/07 Fin
!
!
! ====================================================================
! --- CARAC = 'DPSIDS' :                                        ------
! --- CALCUL DE DPSIDS (6X6) POUR LE MECANISME DEVIATOIRE K (<4) -----
! ====================================================================
! ON NE CALCULE PAS POUR LE CAS ISOTROPE (K=4) CAR DPSIDS = [ 0 ]
    if (carac(1:6) .eq. 'DPSIDS') then
!
        if (k .eq. 4) then
            call utmess('F', 'COMPOR1_2')
        endif
!
        call lcinma(zero, mat)
        if (consol) goto 600
!
!af 15/05/07 Debut
        call lcinma(zero, dsdds)
        call lcinma(zero, sxs)
        call lcinma(zero, sxp)
        call lcinma(zero, pxp)
        call lcinma(zero, pxh)
!
        if (k .gt. 4) then
            kk = k-4
        else
            kk = k
        endif
!
        do 62 i = 1, ndt
            vp(i) = zero
            vhist(i) = zero
 62     continue
!
        do 63 i = 1, ndi
            if (i .ne. (kk)) then
                vp(i) = un
            endif
 63     continue
!
        do 64 i = 1, ndi
            do 64 j = 1, ndi
                if ((i.ne.kk) .and. (j.ne.kk)) then
                    if (i .eq. j) then
                        dsdds(i,j) = d12
                    else
                        dsdds(i,j) = -d12
                    endif
                endif
 64         continue
        dsdds(ndt+1-kk,ndt+1-kk) = un
!
        if (k .lt. 4) then
            call hujksi('KSI   ', mater, r, ksi, iret)
            if (iret .eq. 1) goto 999
!
            do 65 i = 1, ndt
                do 65 j = 1, ndt
                    sxs(i,j) = sigd(i)*sigd(j)
                    sxp(i,j) = sigd(i)*vp(j)
                    pxp(i,j) = vp(i)*vp(j)
 65             continue
!
            if (.not. dila) then
                do 66 i = 1, ndt
                    do 66 j = 1, ndt
                        mat(i,j) = d12*(&
                                   dsdds(i,j)/q - d12* sxs(i,j)/ q**3.d0 - alpha*ksi*(sxp(i,j)/ (&
                                   &q*p)-pxp(i,j)* q/p**2.d0)&
                                   )
 66                 continue
            else
                do 96 i = 1, ndt
                    do 96 j = 1, ndt
                        mat(i,j) = d12*(&
                                   dsdds(i,j)/q - d12* sxs(i,j)/ q**3.d0 - alpha*ksi*sxp(i,j)/ (q&
                                   &*pcoh)&
                                   )
 96                 continue
            endif
!
        else if ((k.gt.4) .and. (k.lt.8)) then
! ---> MECANISME CYCLIQUE DEVIATOIRE
!
            call hujksi('KSI   ', mater, r, ksi, iret)
            if (iret .eq. 1) goto 999
!
            si = un
            do 670 i = 1, ndi
                if (i .ne. (k-4)) then
                    vhist(i) = si*(xk(1)-th(1)*r)
                    si = -si
                endif
670         continue
            vhist(ndt+5-k) = xk(2)-th(2)*r
!
            do 67 i = 1, ndt
                do 67 j = 1, ndt
                    sxs(i,j) = sigdc(i)*sigdc(j)
                    sxp(i,j) = sigd(i)*vp(j)
                    scxp(i,j) = sigdc(i)*vp(j)
                    pxp(i,j) = vp(i)*vp(j)
                    pxh(i,j) = vhist(i)*vp(j)
 67             continue
!
            ps = zero
            sxh = zero
            scxh = zero
            do 671 i = 1, ndt
                ps = ps + sigd(i)*sigdc(i)
                sxh = sxh + sigd(i)*vhist(i)
                scxh = scxh + sigdc(i)*vhist(i)
671         continue
!
            fac = d12*m*(un-b*(un+log((p -ptrac)/pc)))
            do 68 i = 1, ndt
                do 68 j = 1, ndt
                    if ((.not.consol) .and. (.not.dila)) then
                        mat(i,j) = d12/qc*(&
                                   dsdds(i,j)-fac*pxh(i,j))- d14/qc**3.d0*(sxs(i,j)-fac*scxh*scxp&
                                   &(i,j))- alpha*d12*ksi/(p*qc)*(scxp(i,j)+sxp(i,j)- fac*sxh*pxp&
                                   &(i,j)-ps*d12/qc**2.d0*(scxp(i,j)- fac*scxh*pxp(i,j))-ps*d12/p&
                                   &*pxp(i,j)&
                                   )
                    else if ((.not.consol) .and. dila) then
                        mat(i,j) = d12/qc*(&
                                   dsdds(i,j)-fac*pxh(i,j))- d14/qc**3.d0*(sxs(i,j)-fac*scxh*scxp&
                                   &(i,j))- alpha*d12*ksi/(pcoh*qc)*(scxp(i,j)+sxp(i,j)- fac*sxh*&
                                   &pxp(i,j)-ps*d12/qc**2.d0*(scxp(i,j)- fac*scxh*pxp(i,j))&
                                   )
                    else
                        mat(i,j) = zero
                    endif
 68             continue
!
        endif
600     continue
!af 15/05/07 Fin
!
!
! =====================================================================
! --- CARAC = 'DFDS' :                                     ---------
! --- CALCUL DE DFDS (6X1) POUR LE MECANISME DEVIATOIRE K  ---------
! =====================================================================
    else if (carac(1:4) .eq. 'DFDS') then
!
        if (k .le. 8) p = p -ptrac
!
        do 70 i = 1, ndt
            vec(i) = zero
            vhist(i) = zero
 70     continue
!
        if (k .lt. 4) then
!
            if (tract) then
                if (debug) then
                    call tecael(iadzi, iazk24)
                    nomail = zk24(iazk24-1+3) (1:8)
                    write (ifm,'(10(A))')&
     &        'HUJDDD :: LOG(PK/PC) NON DEFINI DANS LA MAILLE ',nomail
                endif
                iret = 1
                goto 999
            endif
!
            do 71 i = 1, ndi
                if (i .ne. k) then
                    vec(i) = d12*m*r*(un-b*(un+log(p/pc)))
                    if (.not.consol) vec(i) = vec(i)+d12*sigd(i) /q
                endif
 71         continue
!
            if (.not.consol) then
                do 72 i = ndi+1, ndt
                    vec(i) = d12*sigd(i)/q
 72             continue
            endif
!
        else if (k .eq. 4) then
!
            do 73 i = 1, ndi
                vec(i) = -d13
 73         continue
!
!af 09/05/07 Debut
!
        else if ((k .gt. 4) .and. (k .lt. 8)) then
! --- MECANISMES DEVIATOIRES CYCLIQUES
            if (tract) then
                if (debug) then
                    call tecael(iadzi, iazk24)
                    nomail = zk24(iazk24-1+3) (1:8)
                    write (ifm,'(10(A))')&
     &        'HUJDDD :: LOG(PK/PC) NON DEFINI DANS LA MAILLE ',nomail
                endif
                iret = 1
                goto 999
            endif
!
            si = un
            do 730 i = 1, ndi
                if (i .ne. (k-4)) then
                    vhist(i) = si*(xk(1)-th(1)*r)
                    si = -si
                endif
730         continue
!
            vhist(ndt+5-k) = xk(2)-th(2)*r
            scxh = zero
            do 731 i = 1, ndt
                scxh = scxh + sigdc(i)*vhist(i)
731         continue
!
            fac = d12*m*(un-b*(1+log(p/pc)))
!
            do 74 i = 1, ndi
                if (i .ne. (k-4)) then
                    if (.not. consol) vec(i) = fac*(r-scxh*d12/qc)+ d12*sigdc(i)/qc
                endif
 74         continue
            if (.not. consol) vec(ndt+5-k)= d12*sigdc(ndt+5-k)/qc
        else if (k .eq. 8) then
            do 77 i = 1, ndi
                if (vin(22) .eq. un) then
                    vec(i)=d13
                else
                    vec(i)=-d13
                endif
 77         continue
!
!af 09/05/07 Fin
!
        else if (k .gt. 8) then
!
            do 78 i = 1, 3
                if (i .ne. (k-8)) vec(i) = d12
 78         continue
!
        endif
!
! =====================================================================
! --- CARAC = 'PSI' :                                     ---------
! --- CALCUL DE PSI (6X1) POUR LE MECANISME DEVIATOIRE K  ---------
! =====================================================================
    else if (carac(1:3) .eq. 'PSI') then
        do 80 i = 1, ndt
            vec(i) = zero
 80     continue
!
        if (k .lt. 4) then
!
            if (tract) then
                if (debug) then
                    call tecael(iadzi, iazk24)
                    nomail = zk24(iazk24-1+3) (1:8)
                    write (ifm,'(10(A))')&
     &        'HUJDDD :: LOG(PK/PC) NON DEFINI DANS LA MAILLE ',nomail
                endif
                iret = 1
                goto 999
            endif
!
            call hujksi('KSI   ', mater, r, ksi, iret)
            if (iret .eq. 1) goto 999
!
            do 81 i = 1, ndi
                if (i .ne. k) then
                    vec(i) = -alpha*ksi*(sin(degr*angdil) + q/pcoh)
                    if (.not.consol) vec(i) = vec(i) + sigd(i) /q/ 2.d0
                endif
 81         continue
!
            if (.not.consol) vec(ndt+1-k) = sigd(ndt+1-k) /q/2.d0
!
        else if (k .eq. 4) then
!
            do 82 i = 1, ndi
                vec(i) = -d13
 82         continue
!
!af 09/05/07 Debut
        else if (k .eq. 8) then
!
            do 83 i = 1, ndi
                if (vin(22) .eq. un) then
                    if (p .gt. zero) then
                        vec(i) = -d13
                    else
                        vec(i) = d13
                    endif
                else
                    if (p .gt. zero) then
                        vec(i) = d13
                    else
                        vec(i) = -d13
                    endif
                endif
 83         continue
!
        else if ((k .gt. 4) .and. (k .lt. 8)) then
! --- MECANISME DEVIATOIRE CYCLIQUE
! --- PREVOIR TEST POUR TRACTION ET DIVISION PAR ZERO
            if (tract) then
                if (debug) then
                    call tecael(iadzi, iazk24)
                    nomail=zk24(iazk24-1+3) (1:8)
                    write (ifm,'(10(A))')&
     &        'HUJDDD :: LOG(PK/PC) NON DEFINI DANS LA MAILLE ',nomail
                endif
                iret = 1
                goto 999
            endif
!
            call hujksi('KSI   ', mater, r, ksi, iret)
            prod = zero
            do 85 i = 1, ndt
                prod = prod + sigd(i)*sigdc(i)
 85         continue
            if (.not.consol) then
                prod = prod / (2.d0*qc)
            else
                prod = zero
            endif
!
            do 84 i = 1, ndi
!
                if (i .ne. (k-4)) then
                    if (.not.consol) then
                        vec(i) = -alpha*ksi*( sin(degr*angdil) + prod/ pcoh) + sigdc(i )/qc/2.d0
                    else
                        vec(i) = -alpha*ksi*sin(degr*angdil)
                    endif
                endif
!
 84         continue
            if (.not.consol) vec(ndt+5-k) = sigdc(ndt+5-k)/qc/2.d0
!
!af 09/05/07 Fin
!
        else if (k .gt. 8) then
            do 86 i = 1, 3
                if (i .ne. (k-8)) vec(i) = d12
 86         continue
        endif
!
    endif
999 continue
end subroutine
