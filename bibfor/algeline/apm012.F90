subroutine apm012(nk, k24rc, ltest, itest, rayonc,&
                  centrc, lraide, lmasse, solveu)
! aslint: disable=W1304
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/aptest.h'
    include 'asterfort/copisd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/preres.h'
    include 'asterfort/resoud.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/wkvect.h'
    include 'blas/zgehrd.h'
    integer :: nk, lraide, lmasse, itest
    real(kind=8) :: rayonc
    complex(kind=8) :: centrc
    logical :: ltest
    character(len=19) :: solveu
    character(len=24) :: k24rc
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     STEPS 0/1/2 OF THE ARGUMENT PRINCIPAL METHOD THAT COUNT THE
!     EIGENVALUES WITHIN A GIVEN SHAPE OF THE COMPLEX PLANE
!     ------------------------------------------------------------------
! IN NK     : IN : SIZE OF THE EIGENVALUE PROBLEM
! OUT K24RC  :K24 : COEFFICIENTS OF THE CHARACTERISTIC POLYNOMIAL
! IN LTEST  : LOG: RUN OF INTERNAL TEST FOR DEBUGING IF LTEST=.TRUE.
! IN ITEST  : IN : NUMBER OF THE TEST FOR DEBUGING
! IN RAYONC  : R8 : RADIUS OF THE DISC OF THE GIVEN SHAPE
! IN CENTRC : C16: CENTRE OF THE DISC
! IN LRAIDE : IN : JEVEUX DESCRIPTOR OF THE STIFFNESS MATRIX
! IN LMASSE : IN : JEVEUX DESCRIPTOR OF THE MASSE MATRIX
! IN SOLVEU : K19: JEVEUX SD OF THE LINEAR SOLVER
!     ------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
!
!
    integer(kind=4) :: nk4, ilo, ihi, lwork4, info4
    integer :: ifm, niv, imata, nk2, nkm1, i, ivect, iwork, vali, ideeq, iret, j
    integer :: imatb, imatc, nkj, lwork, k, ideb, ifin, jm1, im1, iauxh, iauxh1
    integer :: ivalr, ivalm, ihcol, iadia, ibid, idelg, imult, ifin1, imata0
    real(kind=8) :: rauxr, raux1, rauxx, rauxy, rauxm
    complex(kind=8) :: cun, czero, cbd(100), caux1, caux2, caux(100), cbid
    character(len=19) :: numedd, mas19, k19b, rai19
    character(len=24) :: nomrai, nommas
!
!   --- MISCELLANEOUS ---
    call jemarq()
    call infniv(ifm, niv)
    cun=dcmplx(1.d0,0.d0)
    czero=dcmplx(0.d0,0.d0)
!   --- VERBOSE MODE FOR APM STEPS ---
    niv=2
!
!   --- STEP 0: INITIALIZATIONS AND BUILDING OF THE WORKING MATRIX ---
    nk4=nk
    nk2=nk*nk
    nkm1=nk-1
!
    if (.not.ltest) then
!   --- BUILD THE MATRICES M1 AND K1 WHICH ARE THE SAME AS ---
!   --- THE MATRICES M AND K WITHOUT THE CONSTRAINTS       ---
        nommas=zk24(zi(lmasse+1))
        mas19='&&INVER.MASSMATRIX'
        call copisd('MATR_ASSE', 'V', nommas(1:19), mas19)
        call jeveuo(jexnum(mas19//'.VALM', 1), 'E', ivalm)
!
        nomrai=zk24(zi(lraide+1))
        rai19='&&INVER.STIFFMATRIX'
        call copisd('MATR_ASSE', 'V', nomrai(1:19), rai19)
        call jeveuo(jexnum(rai19//'.VALM', 1), 'E', ivalr)
!
        call dismoi('F', 'NOM_NUME_DDL', nomrai, 'MATR_ASSE', ibid,&
                    numedd, iret)
        call jeveuo(numedd(1:14)//'.SMOS.SMHC', 'L', ihcol)
        call jeveuo(numedd(1:14)//'.SMOS.SMDI', 'L', iadia)
        call jeveuo(numedd(1:14)//'.NUME.DELG', 'L', idelg)
        call jeveuo(numedd(1:14)//'.NUME.DEEQ', 'L', ideeq)
!
        rauxx=dble(centrc)
        rauxy=dimag(centrc)
        rauxm=sqrt(rauxx*rauxx+rauxy*rauxy)
        raux1=rauxm+2.d0*rayonc
        ideb=1
        do 18 j = 1, nk
            jm1=j-1
            ifin=zi(iadia+jm1)
            ifin1=ifin-1
!   --- TO FILTER LAGRANGIAN AND PASSIVE PHYSICAL COLUMNS ---
            if (zi(idelg+jm1) .ne. 0) then
!   --- COLUMN CORRESPONDING TO A LAGRANGIAN MULTIPLIER ---
                imult=0
            else
!   --- COLUMN CORRESPONDING TO A PHYSICAL VARIABLE (ACTIVE OR PASSIVE)
!   --- ACTIVE ---
                imult=1
                do 15 i = ideb, ifin
                    im1=i-1
                    iauxh=zi4(ihcol+im1)
                    rauxx=abs(zr(ivalr+im1))
!   --- PASSIVE (ONLY FIXED DOF NOT LINK ONE ) ---
                    if ((zi(idelg+iauxh-1).ne.0) .and. (rauxx.ne.0.d0)) then
                        if ((zi(ideeq-1+2*(iauxh-1)+1).gt.0) .and.&
                            (zi(ideeq-1+2*(iauxh-1)+2).lt.0)) then
                            imult=0
                        endif
                    endif
15              continue
            endif
            do 16 i = ideb, ifin
                im1=i-1
                zr(ivalr+im1)=zr(ivalr+im1)*imult
                zr(ivalm+im1)=zr(ivalm+im1)*imult
16          continue
            if (imult .eq. 0) then
                zr(ivalr+ifin1)=raux1
                zr(ivalm+ifin1)=1.d0
            endif
            ideb=ifin+1
18      continue
!
!   --- FOR DEBUGING ONLY ---
!        CALL UTIMSD(6,2,.FALSE.,.TRUE.,'.NUME.DELG',15,'G')
!        CALL UTIMSD(6,2,.FALSE.,.TRUE.,'.SMOS',15,'G')
!        CALL UTIMSD(6,2,.FALSE.,.TRUE.,'MATASSR',1,'G')
!        CALL UTIMSD(6,2,.FALSE.,.TRUE.,'MATASSM',1,'G')
!        CALL UTIMSD(6,2,.FALSE.,.TRUE.,RAI19,1,'V')
!        CALL UTIMSD(6,2,.FALSE.,.TRUE.,MAS19,1,'V')
!
        k19b=' '
        call preres(solveu, 'V', iret, k19b, mas19,&
                    ibid, 1)
!
!   --- CASE K REAL SYMETRIC ---
        call wkvect('&&APM012.MATRICE.A0', 'V V R', nk2, imata0)
        do 34 i = 1, nk2
            zr(imata0+i-1)=0.d0
34      continue
        ideb=1
        do 38 j = 1, nk
            jm1=j-1
            ifin=zi(iadia+jm1)
            do 36 i = ideb, ifin
                im1=i-1
                iauxh=zi4(ihcol+im1)
                iauxh1=iauxh-1
                rauxr=zr(ivalr+im1)
!   --- UPPER PARTS
                zr(imata0+jm1*nk+iauxh1)=rauxr
!   --- LOWER PARTS
                zr(imata0+nk*iauxh1+jm1)=rauxr
36          continue
            ideb=ifin+1
38      continue
!
!   --- BUILDING OF M-1*K AND STORE IN ZC(IMATA)
        call resoud(mas19, k19b, solveu, k19b, nk,&
                    k19b, k19b, 'V', zr(imata0), cbid,&
                    ' ', .false., 0, iret)
        call wkvect('&&APM012.MATRICE.A', 'V V C', nk2, imata)
        do 41 i = 1, nk2
            im1=i-1
            zc(imata+im1)=zr(imata0+im1)*cun
41      continue
!
!   --- FOR DEBUGGING ONLY
!   --- COMPUTATIONS OF THE EIGENVALUES OF THE                     ---
!   --- WORKING MATRIX A THANKS TO THE LAPACK DGEEV                ---
!        LWORK=10*NK
!        LWORK4=LWORK
!        CALL WKVECT('&&APM012.TEST.DGEEV.WR','V V R',NK,IWR)
!        CALL WKVECT('&&APM012.TEST.DGEEV.WI','V V R',NK,IWI)
!        CALL WKVECT('&&APM012.TEST.DGEEV.WO','V V R',LWORK,IWORK)
!        CALL DGEEV('N','N',NK4,ZR(IMATA0),NK4,ZR(IWR),ZR(IWI),
!     &               RBID,NK4,RBID,NK4,ZR(IWORK),LWORK4,INFO4)
!        WRITE(IFM,*)'**** EIGENVALUE WORKING MATRIX *******'
!        WRITE(IFM,*)'    INFO LAPACK= ',INFO4
!        DO 2 I=1,NK
!          WRITE(IFM,*)I,ZR(IWR-1+I),ZR(IWI-1+I)
!   2    CONTINUE
!        CALL JEDETR('&&APM012.TEST.DGEEV.WR')
!        CALL JEDETR('&&APM012.TEST.DGEEV.WI')
!        CALL JEDETR('&&APM012.TEST.DGEEV.WO')
!
        call jedetr('&&APM012.MATRICE.A0')
!
    else
!   --- FOR TEST ISSUE ONLY ---
!
        call wkvect('&&APM012.MATRICE.A', 'V V C', nk2, imata)
        call aptest(nk, imata, itest, cbd)
    endif
!
!   --- STEP 1: COMPUTING OF THE HESSENBERG FORM OF DENSE MATRIX ---
!   --- ZC(IMATA) THANKS TO THE LAPACK ROUTINE DGEHRD            ---
    call wkvect('&&APM012.ZGEHRD.TAU', 'V V C', nk-1, ivect)
    call wkvect('&&APM012.ZGEHRD.WORK', 'V V C', nk, iwork)
    ilo=1
    ihi=nk4
    call zgehrd(nk4, ilo, ihi, zc(imata), nk4,&
                zc(ivect), zc(iwork), -1, info4)
    if (info4 .eq. 0) then
        lwork4=int(dble(zc(iwork)))
        lwork =int(dble(zc(iwork)))
        call jedetr('&&APM012.ZGEHRD.WORK')
        call wkvect('&&APM012.ZGEHRD.WORK', 'V V C', lwork, iwork)
        call zgehrd(nk4, ilo, ihi, zc(imata), nk4,&
                    zc(ivect), zc(iwork), lwork4, info4)
    endif
    vali=info4
    if (vali .ne. 0) call u2mesi('F', 'ALGELINE4_12', 1, vali)
    call jeexin('&&APM012.ZGEHRD.WORK', iret)
    if (iret .ne. 0) then
        call jedetr('&&APM012.ZGEHRD.TAU')
        call jedetr('&&APM012.ZGEHRD.WORK')
    endif
!
!   --- TO CLARIFY THE SITUATION, ZEROING THE LOWER TRIANGULAR ---
!   --- PART OF THE HESSENBERG MATRIX                          ---
    do 90 j = 1, nk
        do 89 i = 1, nk
            if (i .ge. (j+2)) zc(imata+(j-1)*nk+i-1)=czero
89      continue
90  end do
!
!
!   --- STEP 2: COMPUTATION OF THE COEFFICIENTS OF THE CHARACTERISTIC --
!   --- POLYNOMIAL THANKS TO THE ROMBOUTS ALGORITHM                  ---
!   --- COEFFICIENTS OF THE CHARACTERISTIC POLYNOMIAL AK             ---
!   --- P(X)=A0+A1*X+A2*(X**2)+....+A(NK-1)*(X**(NK-1))+1.D0*X**NK   ---
!   --- WITH AI=ZK(IMATC+I)                                          ---
    k24rc='&&APM012.ROMBOUT.COEFF'
    call wkvect('&&APM012.ROMBOUT.MAT', 'V V C', nk2, imatb)
    call wkvect(k24rc, 'V V C', nk+1, imatc)
    do 100 i = 1, nk2
        zc(imatb+i-1)=czero
100  end do
    do 120 j = nk, 1, -1
        nkj=nk-j
        do 110 i = 1, j
            do 105 k = nkj, 1, -1
                caux1=dconjg(zc(imata+(j-1)*nk+i-1))* zc(imatb+(j+1-1)&
                *nk+k-1)
!
                caux2=dconjg(zc(imata+(j-1)*nk+j+1-1))* zc(imatb+(i-1)&
                *nk+k-1)
                zc(imatb+(i-1)*nk+k+1-1)=caux1-caux2
105          continue
            zc(imatb+(i-1)*nk+1-1)=dconjg(zc(imata+(j-1)*nk+i-1))
110      continue
        do 115 k = 1, nkj
            zc(imatb+(j-1)*nk+k-1)=zc(imatb+(j-1) *nk+k-1)+ zc(imatb+(&
            j+1-1)*nk+k-1)
115      continue
120  end do
    zc(imatc+nk+1-1)=cun
    do 125 i = 1, nk
        nkj=nk-(i-1)
        zc(imatc+i-1)=((-1)**nkj)*zc(imatb+(1-1)*nk+nkj-1)
125  end do
    call jedetr('&&APM012.MATRICE.A')
    call jedetr('&&APM012.ROMBOUT.MAT')
!
    if ((niv.ge.2) .or. ltest) then
        write(ifm,*)'COEFFICIENTS OF ROMBOUT POLYNOMIAL'
        write(ifm,*)'----------------------------------'
        do 150 i = 0, nk
            write(ifm,*)'I/AI ',i,zc(imatc+i)
150      continue
    endif
!
!   --- INTERMEDIARY TEST RESULT ---
    if (ltest) then
        do 130 j = 1, nk
            caux(j)=zc(imatc+nk)
130      continue
        do 132 i = nkm1, 0, -1
            do 131 j = 1, nk
                caux(j)=caux(j)*cbd(j)+zc(imatc+i)
131          continue
132      continue
        do 133 j = 1, nk
            rauxx=dble(caux(j))
            rauxy=dimag(caux(j))
            rauxm=sqrt(rauxx*rauxx+rauxy*rauxy)
            write(ifm,*)'STEP 2: ROOT  I/P(LBD_I)',j,rauxm,caux(j)
133      continue
    endif
!
    call jedema()
end subroutine
