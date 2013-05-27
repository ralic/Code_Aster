subroutine comatr(option, typev, nbproc, rang, vnconv,&
                  dim1i, dim2i, vecti, dim1r, dim2r,&
                  vectr, dim1c, dim2c, vectc)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     COMMUNICATION VIA LE COMMUNICATEUR MPI COURANT D'UNE MATRICE SOIT
!     REELLE, SOIT ENTIERE, SOIT DE CHAR*, SOIT COMPLEXE.
!     ROUTINE CREE POUR LES BESOINS DU PARALLELISME MPI DANS LES MACROS.
!     EN INPUT: SEULES LES VNCONV(RANG) COLONNES (SI OPTION='S') ET
!              LIGNES (SI OPTION='T') SONT SIGNIFIANTES POUR LE PROCES
!              SUS COURANT.
!     EN OUTPUT: TOUS LES PROCESSUS RECUPERENT LA MEME MATRICE. ELLE EST
!              COMPOSEE DES NBPROC PAQUETS DE VNCONV(I) COLONNES
!              (RESP. LIGNES) DE TOUS LES PROCESSUS I. CHAQUE PAQUET EST
!              RANGE DS LA MATRICE PAR ORDRE DE RANG CROISSANT:
!              EN PREMIER LES VNCONV(1) COLONNES OU LIGNES DU PROCESSUS
!              DE RANG 0, PUIS LES VNCONV(2) DE CELUI DE RANG 1...
! ======================================================================
! IN  OPTION  : K1  : 'S' POUR STANDARD , 'T' POUR TRANSPOSE.
! IN  TYPEV   : K1  : 'R' POUR REEL (AVEC VECTR), 'I' POUR ENTIER (AVEC
!                    VECTI) ET 'C' POUR COMPLEXE (VECTC).
! IN  NBPROC  : IS  : ENTIER CORRESPONDANT AU NBRE DE PROCESSUS MPI.
! IN  RANG    : IS  : ENTIER CORRESPONDANT AU RANG DU PROCESSUS MPI.
! IN  DIM1   : IS  : VECTEURS DU NBRE DE LIGNES DE LA MATRICE CONSIDEREE
! IN  DIM2   : IS  : IDEM NBRE DE COLONNES.
! IN  VNCONV  : IS  : VECTEUR DE NBPROC ENTIERS CORRESPONDANT AUX
!                     DECALAGES PAR PROC.
! IN/OUT VECTI: IS  : MATRICE D'ENTIERS A COMMUNIQUER (DIM1I X DIM2I)
! IN/OUT VECTR: R8  : MATRICE REELLE A COMMUNIQUER    (DIM1R X DIM2R)
! IN/OUT VECTC: C8  : MATRICE COMPLEXE A COMMUNIQUER  (DIM1C X DIM2C)
! ======================================================================
! person_in_charge: olivier.boiteau at edf.fr
    implicit none
!
! PARAMETRES D'APPEL
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/mpicm1.h'
    include 'asterfort/vecinc.h'
    include 'asterfort/vecini.h'
    include 'asterfort/vecint.h'
    include 'blas/dnrm2.h'
    integer :: nbproc, rang, dim1i, dim2i, dim1r, dim2r, dim1c, dim2c
    integer :: vnconv(nbproc), vecti(dim1i, *)
    real(kind=8) :: vectr(dim1r, *)
    complex(kind=8) :: vectc(dim1c, *)
    character(len=1) :: option, typev
!
! VARIABLES LOCALES
    integer :: nconv, nconvg, i, j, idecal, iaux1, ibid, izero, idim1, idim2
    integer :: ifm, niv
    integer(kind=4) :: i40, i41
    real(kind=8) :: rzero, rbid
    complex(kind=8) :: czero, cbid, dcmplx
    logical :: ldebug
!
! --- INIT.
    call jemarq()
    call infniv(ifm, niv)
    izero=0
    rzero=0.d0
    czero=dcmplx(0.d0,0.d0)
    ldebug=.false.
!      LDEBUG=.TRUE.
!
! ----------------------------------------------------------------------
! --- VERIF PARAMETRES INPUT
!-----------------------------------------------------------------------
    if ((option.ne.'S') .and. (option.ne.'T')) call assert(.false.)
    if ((typev.ne.'R') .and. (typev.ne.'I') .and. (typev.ne.'C')) call assert(.false.)
    if ((nbproc.lt.1) .or. (rang.lt.0) .or. (rang+1.gt.nbproc)) call assert(.false.)
!
    if (typev .eq. 'I') then
        idim1=dim1i
        idim2=dim2i
    else if (typev.eq.'R') then
        idim1=dim1r
        idim2=dim2r
    else if (typev.eq.'C') then
        idim1=dim1c
        idim2=dim2c
    else
        call assert(.false.)
    endif
!
! ----------------------------------------------------------------------
! --- CALCULS PRELIMINAIRES
!-----------------------------------------------------------------------
! --- NCONV:  NBRE DE PREMIERES COLONNES A DECALER
! --- NCONVG: SOMME DE DECALAGES
! --- IDECAL: DECALAGE POUR LE PROC COURANT
    nconv=vnconv(rang+1)
    nconvg=0
    idecal=0
    do 10 i = 1, nbproc
        if (vnconv(i) .lt. 0) call assert(.false.)
        if ((i-1) .lt. rang) idecal=idecal+vnconv(i)
        nconvg=nconvg+vnconv(i)
10  end do
    if (option .eq. 'S') then
        if (idim2 .ne. nconvg) call assert(.false.)
    else if (option.eq.'T') then
        if (idim1 .ne. nconvg) call assert(.false.)
    endif
!
! --- VERIF INIT.
    if (ldebug) then
        write(ifm,*)'INITIALISATION***************************'
        if ((typev.eq.'R') .and. (option.eq.'S')) then
            i40=idim1
            i41=1
            do 18 j = 1, idim2
                write(ifm,*)j,dnrm2(i40,vectr(1,j),i41)
18          continue
        else if ((typev.eq.'R').and.(option.eq.'T')) then
! --- ON NE FAIT QU'IMPRIMER LES TERMES CAR CERTAINS SONT EN 1.E+308
            do 19 i = 1, idim1
                write(ifm,*)i,(vectr(i,j),j=1,idim2)
19          continue
        else
            write(ifm,*)'! ATTENTION: DEBUG OPTION NON PRISE EN COMPTE !'
        endif
    endif
!
! ----------------------------------------------------------------------
! --- COMMUNICATIONS PROPREMENTS DITES
!-----------------------------------------------------------------------
! --- STEP 1:
! --- POUR LE PROCESSUS COURANT, ON INITIALISE LA FIN DE LA MATRICE
! --- A ZERO: COMME SEULS LES NCONV PREMIERES COLONNES (RESP. LIGNES)
! --- SONT SIGNIFIANTES.
    if (option .eq. 'S') then
        iaux1=idim1*(idim2-nconv)
    else
        iaux1=idim1-nconv
    endif
    if ((option.eq.'S') .and. (iaux1.gt.0)) then
!
        if (typev .eq. 'R') then
            call vecini(iaux1, rzero, vectr(1, nconv+1))
        else if (typev.eq.'I') then
            call vecint(iaux1, izero, vecti(1, nconv+1))
        else if (typev.eq.'C') then
            call vecinc(iaux1, czero, vectc(1, nconv+1))
        endif
!
    else if ((option.eq.'T').and.(iaux1.gt.0)) then
!
        if (typev .eq. 'R') then
            do 20 j = 1, idim2
                call vecini(iaux1, rzero, vectr(nconv+1, j))
20          continue
        else if (typev.eq.'I') then
            do 21 j = 1, idim2
                call vecint(iaux1, izero, vecti(nconv+1, j))
21          continue
        else if (typev.eq.'C') then
            do 22 j = 1, idim2
                call vecinc(iaux1, czero, vectc(nconv+1, j))
22          continue
        endif
!
    endif
!
! --- VERIF STEP 1.
    if (ldebug) then
        write(ifm,*)'STEP 1***************************'
        if ((typev.eq.'R') .and. (option.eq.'S')) then
            i40=idim1
            i41=1
            do 28 j = 1, idim2
                write(ifm,*)j,dnrm2(i40,vectr(1,j),i41)
28          continue
        else if ((typev.eq.'R').and.(option.eq.'T')) then
            do 29 i = 1, idim1
                write(ifm,*)i,(vectr(i,j),j=1,idim2)
29          continue
        else
            write(ifm,*)'! ATTENTION: DEBUG OPTION NON PRISE EN COMPTE !'
        endif
    endif
!
! --- STEP 2:
! --- ON DECALE LES NCONV PREMIERES LIGNES OU COLONNES POUR LES METTRE
! --- BIEN EN PLACE DS LE BUFFER DE COMMUNICATION. ON DECALE EN COMMEN
! --- CANT PAR LES COLONNES OU LES LIGNES LES PLUS ELOIGNEES DE MANIERE
! --- A NE PAS ECRASER DE DONNEES.
    if ((option.eq.'S') .and. (idecal.gt.0)) then
!
        if (typev .eq. 'R') then
            do 30 j = nconv, 1, -1
                do 301 i = 1, idim1
                    vectr(i,j+idecal)=vectr(i,j)
301              continue
30          continue
        else if (typev.eq.'I') then
            do 31 j = nconv, 1, -1
                do 311 i = 1, idim1
                    vecti(i,j+idecal)=vecti(i,j)
311              continue
31          continue
        else if (typev.eq.'C') then
            do 32 j = nconv, 1, -1
                do 321 i = 1, idim1
                    vectc(i,j+idecal)=vectc(i,j)
321              continue
32          continue
        endif
!
    else if ((option.eq.'T').and.(idecal.gt.0)) then
!
        if (typev .eq. 'R') then
            do 34 j = 1, idim2
                do 341 i = nconv, 1, -1
                    vectr(i+idecal,j)=vectr(i,j)
341              continue
34          continue
        else if (typev.eq.'I') then
            do 35 j = 1, idim2
                do 351 i = nconv, 1, -1
                    vecti(i+idecal,j)=vecti(i,j)
351              continue
35          continue
        else if (typev.eq.'C') then
            do 36 j = 1, idim2
                do 361 i = nconv, 1, -1
                    vectc(i+idecal,j)=vectc(i,j)
361              continue
36          continue
        endif
!
    endif
!
! --- VERIF STEP2.
    if (ldebug) then
        write(ifm,*)'STEP 2***************************'
        if ((typev.eq.'R') .and. (option.eq.'S')) then
            i40=idim1
            i41=1
            do 38 j = 1, idim2
                write(ifm,*)j,dnrm2(i40,vectr(1,j),i41)
38          continue
        else if ((typev.eq.'R').and.(option.eq.'T')) then
            do 39 i = 1, idim1
                write(ifm,*)i,(vectr(i,j),j=1,idim2)
39          continue
        else
            write(ifm,*)'! ATTENTION: DEBUG OPTION NON PRISE EN COMPTE !'
        endif
    endif
!
! --- STEP 3:
! --- ON ANNULE LES IDECAL PREMIERES COLONNES OU LIGNES
    if ((option.eq.'S') .and. (idecal.gt.0)) then
!
        iaux1=idim1*idecal
        if (typev .eq. 'R') then
            call vecini(iaux1, rzero, vectr(1, 1))
        else if (typev.eq.'I') then
            call vecint(iaux1, izero, vecti(1, 1))
        else if (typev.eq.'C') then
            call vecinc(iaux1, czero, vectc(1, 1))
        endif
!
    else if ((option.eq.'T').and.(idecal.gt.0)) then
!
        if (typev .eq. 'R') then
            do 40 j = 1, idim2
                call vecini(idecal, rzero, vectr(1, j))
40          continue
        else if (typev.eq.'I') then
            do 41 j = 1, idim2
                call vecint(idecal, izero, vecti(1, j))
41          continue
        else if (typev.eq.'C') then
            do 42 j = 1, idim2
                call vecinc(idecal, czero, vectc(1, j))
42          continue
        endif
!
    endif
!
! --- VERIF STEP3.
    if (ldebug) then
        write(ifm,*)'STEP 3***************************'
        if ((typev.eq.'R') .and. (option.eq.'S')) then
            i40=idim1
            i41=1
            do 48 j = 1, idim2
                write(ifm,*)j,dnrm2(i40,vectr(1,j),i41)
48          continue
        else if ((typev.eq.'R').and.(option.eq.'T')) then
            do 49 i = 1, idim1
                write(ifm,*)i,(vectr(i,j),j=1,idim2)
49          continue
        else
            write(ifm,*)'! ATTENTION: DEBUG OPTION NON PRISE EN COMPTE !'
        endif
    endif
!
! --- STEP 4 FINAL:
! --- ON COMMUNIQUE TOUTE LA MATRICE
    iaux1=idim1*idim2
    if (typev .eq. 'R') then
        call mpicm1('MPI_SUM', 'R', iaux1, ibid, ibid,&
                    vectr(1, 1), cbid)
    else if (typev.eq.'I') then
        call mpicm1('MPI_SUM', 'I', iaux1, ibid, vecti(1, 1),&
                    rbid, cbid)
    else if (typev.eq.'C') then
        call mpicm1('MPI_SUM', 'C', iaux1, ibid, ibid,&
                    rbid, vectc(1, 1))
    endif
!
! --- VERIF FINALIZATION.
    if (ldebug) then
        write(ifm,*)'FINALISATION***************************'
        if ((typev.eq.'R') .and. (option.eq.'S')) then
            i40=idim1
            i41=1
            do 58 j = 1, idim2
                write(ifm,*)j,dnrm2(i40,vectr(1,j),i41)
58          continue
        else if ((typev.eq.'R').and.(option.eq.'T')) then
            do 59 i = 1, idim1
                write(ifm,*)i,(vectr(i,j),j=1,idim2)
59          continue
        else
            write(ifm,*)'! ATTENTION: DEBUG OPTION NON PRISE EN COMPTE !'
        endif
    endif
    call jedema()
end subroutine
