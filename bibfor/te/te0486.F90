subroutine te0486(option, nomte)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/antisy.h'
    include 'asterfort/b1tdb2.h'
    include 'asterfort/btsig.h'
    include 'asterfort/elref4.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jevech.h'
    include 'asterfort/jevete.h'
    include 'asterfort/matdn.h'
    include 'asterfort/provec.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/tecach.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vectan.h'
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UN CHARGEMENT EN PRESSION SUIVEUSE
!          POUR LES PLAQUES ET COQUES
!          MATRICE DE RIGIDITE TANGENTE POUR LES COQUES 3D
!          (TOUJOURS EN PRESSION SUIVEUSE)
!
!          OPTIONS : 'CHAR_MECA_PRSU_R '
!                    'CHAR_MECA_SRCO3D '
!                    'RIGI_MECA_SRCO3D' (COQUES 3D SEULEMENT)
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
! ----------------------------------------------------------------------
    character(len=24) :: valk
! ----------------------------------------------------------------------
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano
    integer :: igeom, ium, iup, ipres, ires, icaco
    integer :: ino, lzi, lzr, iadzi, iazk24, iret
    integer :: i, j, in, kn, ii, komptn, nb1, nb2
    integer :: ivectu, imatun, intsn, npgsn
    integer :: irco3d, ifco3d, itemps, ierz
    real(kind=8) :: pres, presno(9), madn(3, 51), nks1(3, 51), nks2(3, 51)
    real(kind=8) :: a1(3), a2(3), anta1(3, 3), anta2(3, 3), surf(3)
    real(kind=8) :: rigns(2601), pr
    real(kind=8) :: vecta(9, 2, 3), vectn(9, 3), vectpt(9, 2, 3), valpar(4)
    logical :: locapr
    character(len=8) :: nomail, nompar(4)
!     ------------------------------------------------------------------
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PDEPLMR', 'L', ium)
    call jevech('PDEPLPR', 'L', iup)
!
    if (nomte .ne. 'MEC3QU9H' .and. nomte .ne. 'MEC3TR7H') then
!     --- AUTRES ELEMENTS QUE LA COQUE 3 D ---
!
        call elref4(' ', 'RIGI', ndim, nno, nnos,&
                    npg, ipoids, ivf, idfdx, jgano)
!
        call tecach('NNN', 'PPRESSR', 'L', 1, ipres,&
                    iret)
!
        if (ipres .eq. 0) then
            call jevech('PFRCO3D', 'L', ipres)
            call jevech('PCACOQU', 'L', icaco)
        endif
        call jevech('PVECTUR', 'E', ires)
!
        do 10 ino = 0, nno-1
            if (zr(ipres+ino) .ne. 0.d0) then
                call tecael(iadzi, iazk24)
                nomail = zk24(iazk24-1+3)(1:8)
                valk = nomail
                call u2mesg('F', 'ELEMENTS4_92', 1, valk, 0,&
                            0, 0, 0.d0)
            endif
10      continue
!
    else
!     --- ELEMENTS COQUES 3D ---
!
        call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
!
!        --- NOMBRE DE NOEUDS ( NB1 : SERENDIP , NB2 : LAGRANGE )
        nb1 = zi ( lzi - 1 + 1 )
        nb2 = zi ( lzi - 1 + 2 )
!        --- NBRE POINTS INTEGRATIONS (NPGSN : NORMALE )
        npgsn = zi ( lzi - 1 + 4 )
!
!        ---  ( FONCTIONS DE FORMES, DERIVEES ET POIDS )
        call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
!
!        ---- RECUPERATION DES POINTEURS ( E : ECRITURE ) SELON OPTION
        if (option ( 1 : 16 ) .eq. 'CHAR_MECA_SRCO3D' .or. option ( 1 : 16 ) .eq.&
            'CHAR_MECA_SFCO3D') then
!           --- VECTEUR DES FORCES INTERNES
            call jevech('PVECTUR', 'E', ivectu)
        endif
!
        if (option ( 1 : 16 ) .eq. 'RIGI_MECA_SRCO3D' .or. option ( 1 : 16 ) .eq.&
            'RIGI_MECA_SFCO3D') then
!         --- MATRICE TANGENTE DE RIGIDITE ET INITIALISATION
!
!           CALL JEVECH ( 'PMATUUR' , 'E' , IMATUU )
!            POUR UNE MATRICE NON SYMETRIQUE (VOIR AUSSI MECGME)
            call jevech('PMATUNS', 'E', imatun)
            call r8inir(51 * 51, 0.d0, rigns, 1)
        endif
!
!        ---- RECUPERATION DE L ADRESSE DES VARIABLES NODALES TOTALES
        call jevech('PDEPLMR', 'L', ium)
        call jevech('PDEPLPR', 'L', iup)
!
!        ---  REACTUALISATION DE LA GEOMETRIE
!
        do 30 in = 1, nb2-1
            do 20 ii = 1, 3
                zr ( igeom - 1 + 3 * ( in - 1 ) + ii ) = zr ( igeom -&
  1             + 3 * ( in - 1 ) + ii ) + zr ( ium - 1 + 6 * ( in -&
  1             ) + ii ) + zr ( iup - 1 + 6 * ( in - 1 ) + ii )
20          continue
30      continue
!
!        ---- RECUPERATION DU VECTEUR DE PRESSION NODALE A INTERPOLER
        if (option ( 10 : 16 ) .eq. '_SRCO3D') then
            call jevech('PFRCO3D', 'L', irco3d)
            do 40 kn = 1, nb2
                presno ( kn ) = - zr ( irco3d - 1 + ( kn - 1 ) * 7 +&
  3             )
40          continue
!
        else if (option ( 10 : 16 ) .eq. '_SFCO3D') then
            call jevech('PFFCO3D', 'L', ifco3d)
            call jevech('PTEMPSR', 'L', itemps)
            valpar ( 4 ) = zr ( itemps )
            nompar ( 4 ) = 'INST'
            nompar ( 1 ) = 'X'
            nompar ( 2 ) = 'Y'
            nompar ( 3 ) = 'Z'
!
            do 55 i = 1, nb2
                presno(i)=0.d0
55          continue
!
!            GLOBAL = ZK8 ( IFCO3D + 6 ) .EQ. 'GLOBAL'
            locapr = zk8 ( ifco3d + 6 ) .eq. 'LOCAL_PR'
!
            if (locapr) then
!
!             IF (NOMTE.EQ.'MEC3QU9H') THEN
                do 50 in = 0, nb2-1
                    valpar ( 1 ) = zr ( igeom + 3 * in )
                    valpar ( 2 ) = zr ( igeom + 3 * in + 1 )
                    valpar ( 3 ) = zr ( igeom + 3 * in + 2 )
!
                    call fointe('FM', zk8 ( ifco3d + 2 ), 4, nompar, valpar,&
                                pr, ierz)
                    presno ( in+1 ) = pr
                    if (ierz .ne. 0) call u2mess('F', 'ELEMENTS4_1')
!
50              continue
!
!             ENDIF
            endif
!
        endif
!
!        ---- VECTEURS TANGENTS A1 ET A2 AUX NOEUDS NON NORMALISES
!
        call vectan(nb1, nb2, zr(igeom), zr(lzr), vecta,&
                    vectn, vectpt)
!
!        ---- BOUCLE SUR LES POINTS D INTEGRATION NORMALE
        do 80 intsn = 1, npgsn
!
!          ---- VECTEURS DE BASE AUX POINTS DE GAUSS A KSI3 = 0.D0
            call r8inir(3, 0.d0, a1, 1)
            call r8inir(3, 0.d0, a2, 1)
!
!          ---- INTERPOLATIONS PRESSION
!               VECTEURS TANGENTS NON NORMES A1 A2
            pres = 0.d0
!
            do 70 kn = 1, nb2
!
                pres = pres + zr ( lzr - 1 + 459 + 9 * ( intsn - 1 ) + kn ) * presno ( kn)
!
                do 60 ii = 1, 3
!
                    a1 ( ii ) = a1 ( ii ) + zr ( lzr - 1 + 459 + 9 * (&
                    intsn - 1 ) + kn ) * vecta ( kn , 1 , ii )
!
                    a2 ( ii ) = a2 ( ii ) + zr ( lzr - 1 + 459 + 9 * (&
                    intsn - 1 ) + kn ) * vecta ( kn , 2 , ii )
!
60              continue
!
70          continue
!
!          ---- A1 VECTORIEL A2
            call provec(a1, a2, surf)
!
!          --- MATRICE D INTERPOLATION POUR LES DEPLACEMENTS
            call matdn(nb1, zr (lzr), intsn, madn, nks1,&
                       nks2)
!
            if (option ( 1 : 16 ) .eq. 'CHAR_MECA_SRCO3D' .or. option ( 1 : 16 ) .eq.&
                'CHAR_MECA_SFCO3D') then
!
!            --- FORCE EXTERNE NODALE AU SIGNE DU TE0423 ET NMPR3D
                call btsig(6 * nb1 + 3, 3, - pres * zr (lzr - 1 +127 + intsn - 1), madn, surf,&
                           zr ( ivectu ))
            endif
!
            if (option ( 1 : 16 ) .eq. 'RIGI_MECA_SRCO3D' .or. option ( 1 : 16 ) .eq.&
                'RIGI_MECA_SFCO3D') then
!
!            --- MATRICE ANTISYM DE A1 ET DE A2
                call antisy(a1, 1.d0, anta1)
                call antisy(a2, 1.d0, anta2)
!
!            --- PREMIER TERME
                call b1tdb2(madn, nks2, anta1, pres * zr (lzr - 1 + 127 + intsn - 1), 3,&
                            6 * nb1 + 3, rigns)
!
!           --- DEUXIEME TERME
                call b1tdb2(madn, nks1, anta2, - pres * zr (lzr - 1 + 127 + intsn - 1), 3,&
                            6 * nb1 + 3, rigns)
            endif
!
80      continue
!
!
        if (option ( 1 : 16 ) .eq. 'RIGI_MECA_SRCO3D' .or. option ( 1 : 16 ) .eq.&
            'RIGI_MECA_SFCO3D') then
!
!        --- PARTIE SYMETRIQUE DE LA MATRICE TANGENTE
!
            komptn = 0
!            KOMPTU = 0
            do 110 j = 1, 6 * nb1 + 3
!
!            POUR UNE MATRICE NON SYMETRIQUE (VOIR AUSSI MECGME)
                do 90 i = 1, 6*nb1+3
                    zr ( imatun + komptn ) = - rigns((6*nb1+3)*(i-1)+&
                    j)
                    komptn = komptn + 1
90              continue
!
!              DO 100  I = 1 , J
!                 KOMPTU = KOMPTU + 1
!                 ZR ( IMATUU - 1 + KOMPTU ) = -0.5D0 *
!     &              (    RIGNS ( ( 6 * NB1 + 3 ) * ( J - 1 ) + I )
!     &              +  RIGNS ( ( 6 * NB1 + 3 ) * ( I - 1 ) + J )  )
!
!  100         CONTINUE
!
110          continue
!
        endif
!
    endif
!
end subroutine
