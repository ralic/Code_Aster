subroutine te0443(option, nomte)
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
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/jevete.h'
    include 'asterfort/tecach.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/vdefro.h'
    include 'asterfort/vdrep2.h'
    include 'asterfort/vdrepe.h'
    include 'asterfort/vdsiro.h'
    include 'asterfort/vectan.h'
    include 'asterfort/vectgt.h'
    character(len=16) :: option, nomte
!......................................................................
!
!    - FONCTION REALISEE: CHANGEMENT DE REPERE POUR LES COQUE_3D
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                        'REPE_TENS'  :  TENSEURS
!                        'REPE_GENE'  :  QUANTITES GENERALISEES
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano, iret(4)
    integer :: jgeom, jin, jout, jang, np, i, itab(7), iret1, iret2, nbsp
    integer :: vali(2)
!-----------------------------------------------------------------------
    integer :: intsn, j, jcara, k, lzi, lzr, nb1
    integer :: nb2, ncmp, ncpmax, npgsn, nptmax, nspmax
    real(kind=8) :: epais, zero
!-----------------------------------------------------------------------
    parameter    (nptmax=9,ncpmax=8,nspmax=162)
    real(kind=8) :: alpha, beta, s
    real(kind=8) :: matvn1(2, 2, 10), matvg1(2, 2, 10)
    real(kind=8) :: matvn2(2, 2, 10), matvg2(2, 2, 10)
    real(kind=8) :: vecta(9, 2, 3), vectn(9, 3), vectpt(9, 2, 3)
    real(kind=8) :: vectg(2, 3), vectt(3, 3), conin(nptmax*ncpmax*nspmax)
    real(kind=8) :: rep
    character(len=8) :: pain, paout
    zero = 0.0d0
!
    call jevech('PCACOQU', 'L', jcara)
    epais = zr(jcara)
!
    if (option .ne. 'REPE_TENS' .and. option .ne. 'REPE_GENE') then
!C OPTION DE CALCUL INVALIDE
        call assert(.false.)
    endif
!
    if (option .eq. 'REPE_TENS') then
        ncmp = 6
        call tecach('ONO', 'PCOGAIN', 'L', 7, itab,&
                    iret(1))
        call tecach('ONO', 'PCONOIN', 'L', 7, itab,&
                    iret(2))
        call tecach('ONO', 'PDEGAIN', 'L', 7, itab,&
                    iret(3))
        call tecach('ONO', 'PDENOIN', 'L', 7, itab,&
                    iret(4))
        iret1 = iret(1) + iret(2) + iret(3) + iret(4)
        call assert(iret1.eq.6)
!
        if (iret(1) .eq. 0) then
            pain = 'PCOGAIN'
            paout = 'PCOGAOUT'
        else if (iret(2).eq.0) then
            pain = 'PCONOIN'
            paout = 'PCONOOUT'
        else if (iret(3).eq.0) then
            pain = 'PDEGAIN'
            paout = 'PDEGAOUT'
        else if (iret(4).eq.0) then
            pain = 'PDENOIN'
            paout = 'PDENOOUT'
        endif
!
    else if (option.eq.'REPE_GENE') then
        ncmp = 8
        call tecach('ONO', 'PEFGAIN', 'L', 7, itab,&
                    iret(1))
        call tecach('ONO', 'PEFNOIN', 'L', 7, itab,&
                    iret(2))
        call tecach('ONO', 'PDGGAIN', 'L', 7, itab,&
                    iret(3))
        call tecach('ONO', 'PDGNOIN', 'L', 7, itab,&
                    iret(4))
        iret1 = iret(1) + iret(2) + iret(3) + iret(4)
        call assert(iret1.eq.6)
!
        if (iret(1) .eq. 0) then
            pain = 'PEFGAIN'
            paout = 'PEFGAOUT'
        else if (iret(2).eq.0) then
            pain = 'PEFNOIN'
            paout = 'PEFNOOUT'
        else if (iret(3).eq.0) then
            pain = 'PDGGAIN'
            paout = 'PDGGAOUT'
        else if (iret(4).eq.0) then
            pain = 'PDGNOIN'
            paout = 'PDGNOOUT'
        endif
    endif
!
!  APPEL A ELREF4 POUR RECUPERER NNO ET NPG
!
    call elref4(' ', 'MASS', ndim, nno, nnos,&
                npg, ipoids, ivf, idfdx, jgano)
!
    if (pain(4:5) .eq. 'NO') then
        np = nno
    else if (pain(4:5).eq.'GA') then
        np = npg
    endif
!
    call jevech('PGEOMER', 'L', jgeom)
    call jevech('PANGREP', 'L', jang)
    call jevech(pain, 'L', jin)
    call jevech(paout, 'E', jout)
    call tecach('OOO', pain, 'L', 7, itab,&
                iret2)
    nbsp = itab(7)
    if ((nbsp.ne.1) .and. (mod(nbsp,3).ne.0)) then
        call u2mesi('F', 'ELEMENTS5_54', 1, nbsp)
    endif
!
    alpha = zr(jang)
    beta = zr(jang+1)
    rep = zr(jang+2)
    call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
    nb1  =zi(lzi-1+1)
    nb2  =zi(lzi-1+2)
    npgsn=zi(lzi-1+4)
    call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
!
! ---  CALCUL DES MATRICES DE PASSAGE GLOBAL-INTRINSEQUE
!
    call vectan(nb1, nb2, zr(jgeom), zr(lzr), vecta,&
                vectn, vectpt)
!
! --- DETERMINATION DES REPERES  LOCAUX DE L'ELEMENT AUX POINTS
! --- D'INTEGRATION ET STOCKAGE DE CES REPERES DANS LE VECTEUR .DESR :
!     --------------------------------------------------------------
    k = 0
    do 110 intsn = 1, npgsn
        call vectgt(1, nb1, zr(jgeom), zero, intsn,&
                    zr(lzr), epais, vectn, vectg, vectt)
        do 120 j = 1, 3
            do 130 i = 1, 3
                k = k + 1
                zr(lzr+2000+k-1) = vectt(i,j)
!
130          continue
120      continue
110  end do
!
    call assert(ncmp.le.ncpmax)
    call assert(np.le.nptmax)
    vali(1)=nspmax
    vali(2)=nbsp
    if (nbsp .gt. nspmax) call u2mesi('F', 'ELEMENTS5_4', 2, vali)
!
!     LE TABLEAU CONIN A ETE ALLOUE DE FACON STATIQUE POUR
!     OPTIMISER LE CPU CAR LES APPELS A WKVECT DANS LES TE SONT COUTEUX.
!
    call vdrepe(nomte, matvn1, matvg1)
!
!  ON PREND L INVERSE DES MATRICES
!  (CAR ON REVIENT EN REPERE INTRINSEQUE)
!
    if (rep .eq. 0.d0 .or. rep .eq. 2) then
        if (pain(4:5) .eq. 'NO') then
            do 8 i = 1, np
                s = matvn1(1,2,i)
                matvn1(2,1,i) = s
                matvn1(1,2,i) = -s
 8          continue
        else if (pain(4:5).eq.'GA') then
            do 9 i = 1, np
                s = matvg1(1,2,i)
                matvg1(2,1,i) = s
                matvg1(1,2,i) = -s
 9          continue
        endif
    endif
!
    if (rep .eq. 0.d0) then
!
! --- PASSAGE DES CONTRAINTES DU REPERE LOCAL 1
! --- A L'ELEMENT AU REPERE INTRINSEQUE DE LA COQUE
!     ---------------------------------------
        if (option .eq. 'REPE_TENS') then
            if (pain(4:5) .eq. 'NO') then
                call vdsiro(np, nbsp, matvn1, 'IU', 'N',&
                            zr(jin), conin)
            else if (pain(4:5).eq.'GA') then
                call vdsiro(np, nbsp, matvg1, 'IU', 'G',&
                            zr(jin), conin)
            endif
        else if (option.eq.'REPE_GENE') then
            if (pain(4:5) .eq. 'NO') then
                call vdefro(np, matvn1, zr(jin), conin)
            else if (pain(4:5).eq.'GA') then
                call vdefro(np, matvg1, zr(jin), conin)
            endif
        endif
!
! ---  CALCUL DES MATRICES DE PASSAGE DU CHGT DE REPERE
!       -----------------------------------------------
        call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
        call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
        call vdrep2(alpha, beta, zi(lzi), zr(lzr), matvn2,&
                    matvg2)
!
! ---   PASSAGE DES QUANTITES DU REPERE INTRINSEQUE
! ---   A L'ELEMENT AU REPERE LOCAL DE LA COQUE
!       ---------------------------------------
        if (option .eq. 'REPE_TENS') then
            if (pain(4:5) .eq. 'NO') then
                call vdsiro(np, nbsp, matvn2, 'IU', 'N',&
                            conin, zr(jout))
            else if (pain(4:5).eq.'GA') then
                call vdsiro(np, nbsp, matvg2, 'IU', 'G',&
                            conin, zr(jout))
            endif
        else if (option.eq.'REPE_GENE') then
            if (pain(4:5) .eq. 'NO') then
                call vdefro(np, matvn2, conin, zr(jout))
            else if (pain(4:5).eq.'GA') then
                call vdefro(np, matvg2, conin, zr(jout))
            endif
        endif
!
! --- PASSAGE DES CONTRAINTES DU REPERE INTRINSEQUE
! --- A L'ELEMENT AU REPERE LOCAL 1 DE LA COQUE
!     REPERE = 'COQUE_INTR_UTIL'
!     ---------------------------------------
    else if (rep.eq.1.d0) then
!
! --- PASSAGE DES CONTRAINTES DU REPERE INTRINSEQUE
! --- A L'ELEMENT AU REPERE LOCAL 1 DE LA COQUE
!     REPERE = 'COQUE_INTR_UTIL'
!     ---------------------------------------
        if (option .eq. 'REPE_TENS') then
            if (pain(4:5) .eq. 'NO') then
                call vdsiro(np, nbsp, matvn1, 'IU', 'N',&
                            zr(jin), zr(jout))
            else if (pain(4:5).eq.'GA') then
                call vdsiro(np, nbsp, matvg1, 'IU', 'G',&
                            zr(jin), zr(jout))
            endif
        else if (option.eq.'REPE_GENE') then
            if (pain(4:5) .eq. 'NO') then
                call vdefro(np, matvn1, zr(jin), zr(jout))
            else if (pain(4:5).eq.'GA') then
                call vdefro(np, matvg1, zr(jin), zr(jout))
            endif
        endif
!
! --- PASSAGE DES CONTRAINTES DU REPERE LOCAL 1
! --- A L'ELEMENT AU REPERE INTRINSEQUE DE LA COQUE
!     REPERE = 'COQUE_UTIL_INTR'
!     ---------------------------------------
    else if (rep.eq.2.d0) then
        if (option .eq. 'REPE_TENS') then
            if (pain(4:5) .eq. 'NO') then
                call vdsiro(np, nbsp, matvn1, 'IU', 'N',&
                            zr(jin), zr(jout))
            else if (pain(4:5).eq.'GA') then
                call vdsiro(np, nbsp, matvg1, 'IU', 'G',&
                            zr(jin), zr(jout))
            endif
        else if (option.eq.'REPE_GENE') then
            if (pain(4:5) .eq. 'NO') then
                call vdefro(np, matvn1, zr(jin), zr(jout))
            else if (pain(4:5).eq.'GA') then
                call vdefro(np, matvg1, zr(jin), zr(jout))
            endif
        endif
    endif
!
end subroutine
