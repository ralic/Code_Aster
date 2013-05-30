subroutine rc36fp(nbsigr, nocc, situ, sigr, saltij,&
                  nommat, ug, factus)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/limend.h'
    include 'asterfort/rc36f0.h'
    include 'asterfort/rc36f1.h'
    include 'asterfort/rc36f2.h'
    include 'asterfort/rc36f3.h'
    include 'asterfort/rc36f4.h'
    include 'asterfort/rc36f5.h'
    include 'asterfort/rc36f6.h'
    include 'asterfort/rcvale.h'
    include 'asterfort/u2mesg.h'
    integer :: nbsigr, nocc(*), situ(*), sigr(*)
    real(kind=8) :: saltij(*), ug, factus(*)
    character(len=*) :: nommat
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
!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
!
!     CALCUL DU FACTEUR D'USAGE POUR LES SITUATIONS DE PASSAGE
!
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: isk, isl, k, l, nk, nl, n0, i1, i1a4, nsitup, ifm, niv, icompt
    integer :: jspas, nbsg1, nbsg2, nbsg3, nbp12, nbp23, nbp13
    real(kind=8) :: saltm, nadm, ukl, vale(2)
    logical :: trouve, endur, yapass
    integer :: icodre
    character(len=2) :: k2c, k2l
    character(len=3) :: typass
    character(len=8) :: k8b
!     ------------------------------------------------------------------
!
    call infniv(ifm, niv)
!
    call jeveuo('&&RC32SI.PASSAGE_SIT', 'L', jspas)
    call jelira('&&RC32SI.PASSAGE_1_2', 'LONUTI', nbp12, k8b)
    call jelira('&&RC32SI.PASSAGE_2_3', 'LONUTI', nbp23, k8b)
    call jelira('&&RC32SI.PASSAGE_1_3', 'LONUTI', nbp13, k8b)
    nbsg1 = zi(jspas )
    nbsg2 = zi(jspas+1)
    nbsg3 = zi(jspas+2)
!
! --- MISE A ZERO DES LIGNES ET COLONNES DE LA MATRICE SALT
!     S'IL N'EXISTE PAS DE SITUATIONS DE PASSAGE
!
    call rc36f5(nbp12, nbp23, nbp13, nbsigr, nbsg1,&
                nbsg2, nbsg3, saltij)
!
    if (niv .ge. 2) then
        write(ifm,*) 'MATRICE SALT INITIALE'
        write(ifm,1012) ( situ(2*(l-1)+1),situ(2*(l-1)+2),l=1,nbsigr )
        write(ifm,1010) ( nocc(2*(l-1)+1),nocc(2*(l-1)+2),l=1,nbsigr )
        do 100 k = 1, nbsigr
            i1 = 4*nbsigr*(k-1)
            write(ifm,1000) situ(2*(k-1)+1), nocc(2*(k-1)+1), (saltij(&
            i1+4*(l-1)+1),saltij(i1+4*(l-1)+3), l=1,nbsigr)
            write(ifm,1002) situ(2*(k-1)+2), nocc(2*(k-1)+2), (saltij(&
            i1+4*(l-1)+2),saltij(i1+4*(l-1)+4), l=1,nbsigr)
100      continue
    endif
!
    icompt = 0
    ug = 0.d0
!
10  continue
    saltm = 0.d0
    trouve = .false.
!
! --- RECHERCHE DU SALT MAXI
!
    call rc36f0(nbsigr, nocc, saltij, saltm, trouve,&
                isk, isl, i1a4, nk, nl)
!
    if (.not. trouve) goto 9999
!
! --- DETERMINATION DU N0
!     RECHERCHE DES CHEMINS DE PASSAGE
!
    call rc36f1(nbsigr, nocc, saltij, isk, isl,&
                nk, nl, n0, nbp12, nbp23,&
                nbp13, sigr, yapass, typass, nsitup)
!
    call limend(nommat, saltm, 'WOHLER', k8b, endur)
    if (endur) then
        ukl = 0.d0
    else
        call rcvale(nommat, 'FATIGUE', 1, 'SIGM    ', saltm,&
                    1, 'WOHLER  ', nadm, icodre, 2)
        if (nadm .lt. 0) then
            vale(1) = saltm
            vale(2) = nadm
            call u2mesg('A', 'POSTRCCM_32', 0, ' ', 0,&
                        0, 2, vale)
        endif
        ukl = dble( n0 ) / nadm
    endif
!
    if (icompt .le. 49) then
        icompt = icompt + 1
        factus(4*(icompt-1)+1) = i1a4
        factus(4*(icompt-1)+2) = situ(2*(isk-1)+1)
        factus(4*(icompt-1)+3) = situ(2*(isl-1)+1)
        factus(4*(icompt-1)+4) = ukl
    endif
!
    if (niv .ge. 2) then
        if (i1a4 .eq. 1 .or. i1a4 .eq. 3) then
            k2l = '_A'
        else
            k2l = '_B'
        endif
        if (i1a4 .eq. 1 .or. i1a4 .eq. 2) then
            k2c = '_A'
        else
            k2c = '_B'
        endif
        if (yapass) then
            write(ifm,1040)'=> SALT MAXI = ', saltm, situ(2*(isk-1)+1)&
            , k2l, situ(2*(isl-1)+1), k2c, typass, situ(2*(nsitup-1)+&
            1)
        else
            write(ifm,1042)'=> SALT MAXI = ', saltm, situ(2*(isk-1)+1)&
            , k2l, situ(2*(isl-1)+1), k2c
        endif
        write(ifm,1030)'          N0 = ', n0
        write(ifm,1020)'        NADM = ', nadm
        write(ifm,1020)'         UKL = ', ukl
    endif
!
! --- MISE A ZERO DES LIGNES ET COLONNES DE LA MATRICE SALT SUIVANT
!     LE NOMBRE D'OCCURENCE EGAL A ZERO
!
    call rc36f2(nbsigr, nocc, saltij, i1a4, isk,&
                isl, nk, nl, n0)
!
! --- IDEM POUR LE CHEMIN DE PASSAGE
!
    if (yapass) then
        if (nocc(2*(nsitup-1)+1) .ne. 0) then
            nocc(2*(nsitup-1)+1) = max(0,nocc(2*(nsitup-1)+1)-n0)
        else
            nocc(2*(nsitup-1)+2) = max(0,nocc(2*(nsitup-1)+2)-n0)
        endif
        if (nocc(2*(nsitup-1)+1) .eq. 0 .and. nocc(2*(nsitup-1)+2) .eq. 0) then
            if (typass .eq. '1_2') then
                nbp12 = nbp12 - 1
            else if (typass .eq. '1_3') then
                nbp13 = nbp13 - 1
            else if (typass .eq. '2_3') then
                nbp23 = nbp23 - 1
            endif
        endif
        call rc36f3(nbsigr, nocc, saltij, nsitup)
        call rc36f4(typass, nbp12, nbp23, nbp13, nbsigr,&
                    nbsg1, nbsg2, nbsg3, saltij)
    endif
!
! --- ON VERIFIE SI LA COMBINAISON A ANNULEE DES CHEMINS DE PASSAGE
    call rc36f6(nbp12, nbp23, nbp13, nbsigr, nbsg1,&
                nbsg2, nbsg3, sigr, nocc, saltij)
!
    if (niv .ge. 2) then
        write(ifm,*) 'MATRICE SALT MODIFIEE'
        write(ifm,1012) ( situ(2*(l-1)+1),situ(2*(l-1)+2),l=1,nbsigr )
        write(ifm,1010) ( nocc(2*(l-1)+1),nocc(2*(l-1)+2),l=1,nbsigr )
        do 110 k = 1, nbsigr
            i1 = 4*nbsigr*(k-1)
            write(ifm,1000) situ(2*(k-1)+1), nocc(2*(k-1)+1), (saltij(&
            i1+4*(l-1)+1),saltij(i1+4*(l-1)+3), l=1,nbsigr)
            write(ifm,1002) situ(2*(k-1)+2), nocc(2*(k-1)+2), (saltij(&
            i1+4*(l-1)+2),saltij(i1+4*(l-1)+4), l=1,nbsigr)
110      continue
    endif
!
    ug = ug + ukl
    goto 10
!
9999  continue
!
    1000 format(1p,i7,'_A',i9,'|',40(e9.2,1x,e9.2,'|'))
    1002 format(1p,i7,'_B',i9,'|',40(e9.2,1x,e9.2,'|'))
    1010 format(1p,9x,'NB_OCCUR ','|',40(i9,1x,i9,'|'))
    1012 format(1p,9x,'SITUATION','|',40(i7,'_A',1x,i7,'_B|'))
    1040 format(1p,a15,e12.5,', LIGNE:',i4,a2,', COLONNE:',i4,a2,&
     &       ', PASSAGE: ',a3,', SITUATION DE PASSAGE: ',i4)
    1042 format(1p,a15,e12.5,', LIGNE:',i4,a2,', COLONNE:',i4,a2)
    1030 format(1p,a15,i12)
    1020 format(1p,a15,e12.5)
!
end subroutine
