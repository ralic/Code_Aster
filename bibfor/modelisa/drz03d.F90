subroutine drz03d(lisnoz, lonlis, chargz, typlaz, lisrez,&
                  dmin)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRP_20
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterfort/afrela.h'
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/matinv.h'
    include 'asterfort/pmat.h'
    include 'asterfort/pmppr.h'
    include 'asterfort/provec.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: charge
    character(len=19) :: lisrel
    character(len=24) :: lisnoe
    character(len=*) :: chargz, lisnoz, typlaz, lisrez
    real(kind=8) :: dmin
! -------------------------------------------------------
!     BLOCAGE DES DEPLACEMENTS RELATIFS D'UNE LISTE DE NOEUDS
!     SPECIFIEE PAR L'UTILISATEUR DANS LE CAS OU L'ON EST
!     EN 3D ET AUCUN  NOEUD NE PORTE LE DDL DRZ
! -------------------------------------------------------
!  LISNOE - IN    - K24 - : NOM DE LA LISTE DES
!         -       -     -   NOEUDS A LIER
! ------------------------------------------------
!  LONLIS - IN    - I   - : LONGUEUR DE LA LISTE DES
!         -       -     -   NOEUDS A LIER
! ------------------------------------------------
!  CHARGE - IN    - K8   - : NOM DE LA SD CHARGE
!         - JXIN  -      -
! ------------------------------------------------
! TYPLAG  - IN    - K2  - : TYPE DES MULTIPLICATEURS DE LAGRANGE
!                           ASSOCIES A LA RELATION :
!                       SI = '12'  LE PREMIER LAGRANGE EST AVANT
!                                  LE NOEUD PHYSIQUE
!                                  LE SECOND LAGRANGE EST APRES
!                       SI = '22'  LE PREMIER LAGRANGE EST APRES
!                                  LE NOEUD PHYSIQUE
!                                  LE SECOND LAGRANGE EST APRES
! ------------------------------------------------
!  LISREL - IN    - K19  - : NOM DE LA SD
!         - JXVAR -      -   LISTE DE RELATIONS
! -------------------------------------------------------
!  DMIN   - IN    - R8 - : LONGUEUR EN DESSOUS DE LAQUELLE ON CONSIDERE
!                          QUE 2 POINTS SONT CONFONDUS
! -------------------------------------------------------
!
!
! --------- VARIABLES LOCALES ---------------------------
!-----------------------------------------------------------------------
    integer :: i, ibid, ier, ierd, ilisno, inoa, inom
    integer :: itrian, j, jcoor, jliscc, jliscr, jlisdi, jlisdl
    integer :: jlisdm, jlisno, jnoma, lonlis, nbcmp, nbec, nbterm
    integer :: nddla, nmocl
    real(kind=8) :: beta, un, zero
!-----------------------------------------------------------------------
    parameter (nmocl=300)
    complex(kind=8) :: betac
    character(len=2) :: typlag
    character(len=4) :: typval, typcoe
    character(len=8) :: betaf, resu
    character(len=8) :: mod, nomg, k8bid
    character(len=8) :: noma, nomcmp(nmocl)
    character(len=8) :: noeua, noeub, noeuc
    character(len=9) :: nomte
    character(len=16) :: type, oper
    character(len=19) :: ligrmo
    integer :: ntypel(nmocl), j2, k, k2, inob
    integer :: inoc, inoem
    integer :: vali(2), i2, inoi, ino2, ialign, j1
    real(kind=8) :: m1(3, 3), minv1(3, 3), m2(3, 12), m3(3, 3), r8bid
    real(kind=8) :: m4(3, 12), m5(3, 12), x1, y1, z1
    real(kind=8) :: b(3), c(3), m(3), n(3), bn(3), cn(3)
    real(kind=8) :: ml1(3, 9), ml3(3, 3), ml5(3, 9), xi, yi, zij
    real(kind=8) :: n1(3), n2(3), abm(3), coek, coek1, lab, sabc, lac
    character(len=1) :: k1bid
! --------- FIN  DECLARATIONS  VARIABLES LOCALES --------
    call jemarq()
    call getres(resu, type, oper)
    lisrel = lisrez
    charge = chargz
    typlag = typlaz
    lisnoe = lisnoz
!
! --- INITIALISATIONS
!
    betaf = '&FOZERO'
    beta = 0.0d0
    betac = (0.0d0,0.0d0)
    un = 1.0d0
    zero = 0.0d0
    call assert(dmin .gt. 0.d0)
!
    call dismoi('F', 'NOM_MODELE', charge(1:8), 'CHARGE', ibid,&
                mod, ier)
    ligrmo = mod(1:8)//'.MODELE'
    call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
    noma = zk8(jnoma)
    typcoe = 'REEL'
!
! --- TYPE DES VALEURS AU SECOND MEMBRE DES RELATIONS
    if (oper(15:16) .eq. '_F') then
        typval = 'FONC'
    else if (oper(15:16).eq.'_C') then
        typval = 'COMP'
    else if (oper(15:16).eq.'  ') then
        typval = 'REEL'
    else
        call assert(.false.)
    endif
!
! --- RECUPERATION DES NOMS DES DDLS ET DES NUMEROS
! --- D'ELEMENTS DE LAGRANGE ASSOCIES
!
    nomg = 'DEPL_R'
    nomte = 'D_DEPL_R_'
!
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg), 'L', inom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nbcmp, k1bid)
    nddla = nbcmp - 1
    if (nddla .gt. nmocl) then
        vali (1) = nmocl
        vali (2) = nddla
        call u2mesg('F', 'MODELISA8_29', 0, ' ', 2,&
                    vali, 0, 0.d0)
    endif
    do 10 i = 1, nddla
        nomcmp(i) = zk8(inom-1+i)
        call jenonu(jexnom('&CATA.TE.NOMTE', nomte//nomcmp(i) (1:7)), ntypel(i))
10  end do
    call dismoi('F', 'NB_EC', nomg, 'GRANDEUR', nbec,&
                k8bid, ierd)
!
    if (nbec .gt. 10) then
        call u2mess('F', 'MODELISA_94')
    endif
!
! --- CREATION DES TABLEAUX DE TRAVAIL NECESSAIRES A L'AFFECTATION
! --- DE LISREL
!
! ---  MAJORANT DU NOMBRE DE TERMES DANS UNE RELATION
    nbterm = 12
! ---  VECTEUR DU NOM DES NOEUDS
    call wkvect('&&DRZ03D.LISNO', 'V V K8', nbterm, jlisno)
! ---  VECTEUR DU NOM DES DDLS
    call wkvect('&&DRZ03D.LISDDL', 'V V K8', nbterm, jlisdl)
! ---  VECTEUR DES COEFFICIENTS REELS
    call wkvect('&&DRZ03D.COER', 'V V R', nbterm, jliscr)
! ---  VECTEUR DES COEFFICIENTS COMPLEXES
    call wkvect('&&DRZ03D.COEC', 'V V C', nbterm, jliscc)
! ---  VECTEUR DES DIRECTIONS DES DDLS A CONTRAINDRE
    call wkvect('&&DRZ03D.DIRECT', 'V V R', 3*nbterm, jlisdi)
! ---  VECTEUR DES DIMENSIONS DE CES DIRECTIONS
    call wkvect('&&DRZ03D.DIME', 'V V I', nbterm, jlisdm)
!
! --- RECUPERATION DU TABLEAU DES COORDONNEES
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
! --- ACQUISITION DE LA LISTE DES NOEUDS A LIER
! --- (CETTE LISTE EST NON REDONDANTE)
    call jeveuo(lisnoe, 'L', ilisno)
!
!
! --- INITIALISATIONS
    do 30 i = 1, 3
        do 20 j = 1, 12
            m2(i,j) = zero
            m4(i,j) = zero
            m5(i,j) = zero
20      continue
30  end do
!
    do 50 i = 1, 3
        do 40 j = 1, 3
            m1(i,j) = zero
            minv1(i,j) = zero
            m3(i,j) = zero
40      continue
50  end do
!
!
!
! ---  RECHERCHE DE NOEUDS A, B, C FORMANT UN TRIANGLE
! ---  DE SURFACE NON-NULLE.
! ---  ON PREND POUR A LE PREMIER NOEUD
! ---           POUR B LE PREMIER NOEUD DISTINCT DE A
! ---           POUR C LE PREMIER NOEUD DISTINCT DE A ET B
! ---                  ET NON COLINEAIRE A AB.
!
    noeua = zk8(ilisno+1-1)
    call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+1-1)), inoa)
    itrian = 0
    do 70 j = 2, lonlis
!
        call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+j-1)), inob)
!
        b(1) = zr(jcoor-1+3* (inob-1)+1) - zr(jcoor-1+3* (inoa-1)+1)
        b(2) = zr(jcoor-1+3* (inob-1)+2) - zr(jcoor-1+3* (inoa-1)+2)
        b(3) = zr(jcoor-1+3* (inob-1)+3) - zr(jcoor-1+3* (inoa-1)+3)
!       -- LAB : LONGUEUR AB
        lab=sqrt(b(1)*b(1)+b(2)*b(2)+b(3)*b(3))
!
        j2 = j
        if (lab .le. dmin) goto 70
!
        noeub = zk8(ilisno+j2-1)
        do 60 k = j2 + 1, lonlis
            call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+k-1)), inoc)
!
            c(1) = zr(jcoor-1+3* (inoc-1)+1) - zr(jcoor-1+3* (inoa-1)+ 1)
            c(2) = zr(jcoor-1+3* (inoc-1)+2) - zr(jcoor-1+3* (inoa-1)+ 2)
            c(3) = zr(jcoor-1+3* (inoc-1)+3) - zr(jcoor-1+3* (inoa-1)+ 3)
            lac=sqrt(c(1)*c(1)+c(2)*c(2)+c(3)*c(3))
            if (lac .le. dmin) goto 60
!
!         -- CALCUL DE SABC : 2*SURFACE ABC
            call provec(b, c, n)
            sabc=sqrt(n(1)*n(1)+n(2)*n(2)+n(3)*n(3))
!
            if (sabc/max(lab,lac) .le. dmin) then
                goto 60
            else
                k2 = k
                noeuc = zk8(ilisno+k2-1)
                itrian = 1
                goto 80
            endif
60      continue
70  end do
80  continue
!
!
!
! ---  1) CAS OU L'ON A PU TOUVER 3 NOEUDS FORMANT UN TRIANGLE
! ---     DE SURFACE NON NULLE
!--------------------------------------------------------------
    if (itrian .eq. 1) then
!
! ---   CALCUL DE BN = B X N
        call provec(b, n, bn)
!
! ---   CALCUL DE CN = C X N
        call provec(c, n, cn)
!
! ---     DEFINITION DE M1
!
        m1(1,1) = bn(1)
        m1(1,2) = bn(2)
        m1(1,3) = bn(3)
        m1(2,1) = cn(1)
        m1(2,2) = cn(2)
        m1(2,3) = cn(3)
        m1(3,1) = n(1)
        m1(3,2) = n(2)
        m1(3,3) = n(3)
!
! ---     DEFINITION DE M2
!
        m2(1,1) = -n(1)
        m2(1,2) = -n(2)
        m2(1,3) = -n(3)
        m2(1,4) = n(1)
        m2(1,5) = n(2)
        m2(1,6) = n(3)
        m2(2,1) = -n(1)
        m2(2,2) = -n(2)
        m2(2,3) = -n(3)
        m2(2,7) = n(1)
        m2(2,8) = n(2)
        m2(2,9) = n(3)
        m2(3,1) = -c(1)
        m2(3,2) = -c(2)
        m2(3,3) = -c(3)
        m2(3,4) = c(1)
        m2(3,5) = c(2)
        m2(3,6) = c(3)
!
! ---     DEFINITION DE M4
!
        m4(1,1) = -un
        m4(2,2) = -un
        m4(3,3) = -un
        m4(1,10) = un
        m4(2,11) = un
        m4(3,12) = un
!
! ---     INVERSION DE M1
!
        call matinv('S', 3, m1, minv1, r8bid)
!
! ---     PREMIERE RELATION POUR LES NOEUDS B ET C :
! ---     (UB-UA).B = 0
!
        nbterm = 6
!
        zk8(jlisno+1-1) = noeub
        zk8(jlisno+2-1) = noeua
        zk8(jlisno+3-1) = noeub
        zk8(jlisno+4-1) = noeua
        zk8(jlisno+5-1) = noeub
        zk8(jlisno+6-1) = noeua
!
        zk8(jlisdl+1-1) = 'DX'
        zk8(jlisdl+2-1) = 'DX'
        zk8(jlisdl+3-1) = 'DY'
        zk8(jlisdl+4-1) = 'DY'
        zk8(jlisdl+5-1) = 'DZ'
        zk8(jlisdl+6-1) = 'DZ'
!
        zr(jliscr+1-1) = b(1)
        zr(jliscr+2-1) = -b(1)
        zr(jliscr+3-1) = b(2)
        zr(jliscr+4-1) = -b(2)
        zr(jliscr+5-1) = b(3)
        zr(jliscr+6-1) = -b(3)
!
        call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                    zr(jlisdi), nbterm, beta, betac, betaf,&
                    typcoe, typval, typlag, 0.d0, lisrel)
!
! ---     DEUXIEME RELATION POUR LES NOEUDS B ET C :
! ---     (UC-UA).C = 0
!
        nbterm = 6
!
        zk8(jlisno+1-1) = noeuc
        zk8(jlisno+2-1) = noeua
        zk8(jlisno+3-1) = noeuc
        zk8(jlisno+4-1) = noeua
        zk8(jlisno+5-1) = noeuc
        zk8(jlisno+6-1) = noeua
!
        zr(jliscr+1-1) = c(1)
        zr(jliscr+2-1) = -c(1)
        zr(jliscr+3-1) = c(2)
        zr(jliscr+4-1) = -c(2)
        zr(jliscr+5-1) = c(3)
        zr(jliscr+6-1) = -c(3)
!
        call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                    zr(jlisdi), nbterm, beta, betac, betaf,&
                    typcoe, typval, typlag, 0.d0, lisrel)
!
! ---     TROISIEME RELATION POUR LES NOEUDS B ET C :
! ---     (UB-UA).C + (UC-UA).B = 0
!
        nbterm = 9
!
        zk8(jlisno+1-1) = noeua
        zk8(jlisno+2-1) = noeub
        zk8(jlisno+3-1) = noeuc
        zk8(jlisno+4-1) = noeua
        zk8(jlisno+5-1) = noeub
        zk8(jlisno+6-1) = noeuc
        zk8(jlisno+7-1) = noeua
        zk8(jlisno+8-1) = noeub
        zk8(jlisno+9-1) = noeuc
!
        zk8(jlisdl+1-1) = 'DX'
        zk8(jlisdl+2-1) = 'DX'
        zk8(jlisdl+3-1) = 'DX'
        zk8(jlisdl+4-1) = 'DY'
        zk8(jlisdl+5-1) = 'DY'
        zk8(jlisdl+6-1) = 'DY'
        zk8(jlisdl+7-1) = 'DZ'
        zk8(jlisdl+8-1) = 'DZ'
        zk8(jlisdl+9-1) = 'DZ'
!
        zr(jliscr+1-1) = -b(1) - c(1)
        zr(jliscr+2-1) = c(1)
        zr(jliscr+3-1) = b(1)
        zr(jliscr+4-1) = -b(2) - c(2)
        zr(jliscr+5-1) = c(2)
        zr(jliscr+6-1) = b(2)
        zr(jliscr+7-1) = -b(3) - c(3)
        zr(jliscr+8-1) = c(3)
        zr(jliscr+9-1) = b(3)
!
        call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                    zr(jlisdi), nbterm, beta, betac, betaf,&
                    typcoe, typval, typlag, 0.d0, lisrel)
!
        do 150 j = 2, lonlis
!
            call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+j-1)), inoem)
!
            if (inoem .eq. inob .or. inoem .eq. inoc) goto 150
            m(1) = zr(jcoor-1+3* (inoem-1)+1) - zr(jcoor-1+3* (inoa-1) +1)
            m(2) = zr(jcoor-1+3* (inoem-1)+2) - zr(jcoor-1+3* (inoa-1) +2)
            m(3) = zr(jcoor-1+3* (inoem-1)+3) - zr(jcoor-1+3* (inoa-1) +3)
!
! ---        DEFINITION DE M3
!
            m3(1,2) = -m(3)
            m3(1,3) = m(2)
            m3(2,1) = m(3)
            m3(2,3) = -m(1)
            m3(3,1) = -m(2)
            m3(3,2) = m(1)
!
! ---        CALCUL DE M1 <-- M3.MINV1
!
            call pmat(3, m3, minv1, m1)
!
! ---        CALCUL DE M5 <-- M3.MINV1.M2
!
            call pmppr(m1, 3, 3, 1, m2,&
                       3, 12, 1, m5, 3,&
                       12)
!
! ---        CALCUL DE M5 <-- M4 + M3.MINV1.M2
!
            do 100 j1 = 1, 3
                do 90 j2 = 1, 12
                    m5(j1,j2) = m5(j1,j2) + m4(j1,j2)
90              continue
100          continue
!
! ---     ECRITURE DES 3 RELATIONS CORRESPONDANTES A M5.UABCM = 0
!
            nbterm = 12
!
            do 110 k = 1, 3
                zk8(jlisno+k-1) = noeua
                zk8(jlisno+3+k-1) = noeub
                zk8(jlisno+6+k-1) = noeuc
                zk8(jlisno+9+k-1) = zk8(ilisno+j-1)
110          continue
!
            do 120 k = 1, 4
                zk8(jlisdl+3* (k-1)+1-1) = 'DX'
                zk8(jlisdl+3* (k-1)+2-1) = 'DY'
                zk8(jlisdl+3* (k-1)+3-1) = 'DZ'
120          continue
!
            do 140 j1 = 1, 3
                do 130 j2 = 1, 12
                    zr(jliscr+j2-1) = m5(j1,j2)
130              continue
!
                call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8( jlisno), zi(jlisdm),&
                            zr(jlisdi), nbterm, beta, betac, betaf,&
                            typcoe, typval, typlag, 0.d0, lisrel)
140          continue
! ---     FIN DE LA BOUCLE SUR LES NOEUDS DE LA LISTE
150      continue
!
!
!
! ---   2)  CAS OU L'ON N'A PAS PU TROUVER 3 NOEUDS DE LA
! ---        LISTE FORMANT UN TRIANGLE
!--------------------------------------------------------------
!
    else if (itrian.eq.0) then
!
        x1 = zr(jcoor-1+3* (inoa-1)+1)
        y1 = zr(jcoor-1+3* (inoa-1)+2)
        z1 = zr(jcoor-1+3* (inoa-1)+3)
!
        ialign = 0
!
        do 160 i = 2, lonlis
            call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+i-1)), inoi)
            xi = zr(jcoor-1+3* (inoi-1)+1)
            yi = zr(jcoor-1+3* (inoi-1)+2)
            zij = zr(jcoor-1+3* (inoi-1)+3)
            if ((abs(xi-x1).gt.dmin) .or. (abs(yi-y1).gt.dmin) .or. (abs(zij-z1).gt.dmin)) then
                ialign = 1
                goto 200
            endif
160      continue
!
!
!
! ---   3) CAS OU TOUS LES NOEUDS DE LA LISTE ONT LES MEMES COORDONNEES
!-----------------------------------------------------------------------
!
! ---     PREMIERE RELATION
! ---     DX(M) -DX(A) = 0
!
        nbterm = 2
        zk8(jlisno+1-1) = zk8(ilisno+1-1)
        zk8(jlisdl+1-1) = 'DX'
        zr(jliscr+1-1) = un
!
        do 170 i = 2, lonlis
            zk8(jlisno+2-1) = zk8(ilisno+i-1)
            zk8(jlisdl+2-1) = 'DX'
            zr(jliscr+2-1) = -un
!
            call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                        zr(jlisdi), nbterm, beta, betac, betaf,&
                        typcoe, typval, typlag, 0.d0, lisrel)
170      continue
!
! ---     DEUXIEME RELATION
! ---     DY(M) -DY(A) = 0
!
        zk8(jlisdl+1-1) = 'DY'
!
        do 180 i = 2, lonlis
            zk8(jlisno+2-1) = zk8(ilisno+i-1)
            zk8(jlisdl+2-1) = 'DY'
!
            call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                        zr(jlisdi), nbterm, beta, betac, betaf,&
                        typcoe, typval, typlag, 0.d0, lisrel)
180      continue
!
! ---     TROISIEME RELATION
! ---     DZ(M) -DZ(A) = 0
!
        zk8(jlisdl+1-1) = 'DZ'
!
        do 190 i = 2, lonlis
            zk8(jlisno+2-1) = zk8(ilisno+i-1)
            zk8(jlisdl+2-1) = 'DZ'
!
            call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                        zr(jlisdi), nbterm, beta, betac, betaf,&
                        typcoe, typval, typlag, 0.d0, lisrel)
190      continue
!
200      continue
!
!
!
! ---   4) CAS OU TOUS LES NOEUDS SONT ALIGNES
!--------------------------------------------------------------
        if (ialign .eq. 1) then
!
! ---           CAS OU L'ON N'A QUE 2 POINTS, LA SEULE RELATION EST :
! ---           (UB-UA).B = 0
!
            if (lonlis .eq. 2) then
!
                nbterm = 6
!
                zk8(jlisno+1-1) = noeub
                zk8(jlisno+2-1) = noeua
                zk8(jlisno+3-1) = noeub
                zk8(jlisno+4-1) = noeua
                zk8(jlisno+5-1) = noeub
                zk8(jlisno+6-1) = noeua
!
                zk8(jlisdl+1-1) = 'DX'
                zk8(jlisdl+2-1) = 'DX'
                zk8(jlisdl+3-1) = 'DY'
                zk8(jlisdl+4-1) = 'DY'
                zk8(jlisdl+5-1) = 'DZ'
                zk8(jlisdl+6-1) = 'DZ'
!
                zr(jliscr+1-1) = b(1)
                zr(jliscr+2-1) = -b(1)
                zr(jliscr+3-1) = b(2)
                zr(jliscr+4-1) = -b(2)
                zr(jliscr+5-1) = b(3)
                zr(jliscr+6-1) = -b(3)
!
                call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8( jlisno), zi(jlisdm),&
                            zr(jlisdi), nbterm, beta, betac, betaf,&
                            typcoe, typval, typlag, 0.d0, lisrel)
            else if (lonlis.gt.2) then
!
! ---               INITIALISATIONS
!
                do 220 i = 1, 3
                    do 210 j = 1, 9
                        ml1(i,j) = zero
                        ml5(i,j) = zero
210                  continue
220              continue
!
                do 240 i = 1, 3
                    do 230 j = 1, 3
                        ml3(i,j) = zero
230                  continue
240              continue
!
                do 250 i = 2, lonlis
                    call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+i-1) ), inob)
                    b(1) = zr( jcoor-1+3* (inob-1)+1) - zr(jcoor-1+3* ( inoa-1)+1)
                    b(2) = zr( jcoor-1+3* (inob-1)+2) - zr(jcoor-1+3* ( inoa-1)+2)
                    b(3) = zr( jcoor-1+3* (inob-1)+3) - zr(jcoor-1+3* ( inoa-1)+3)
                    if (sqrt(b(1)*b(1)+b(2)*b(2)+b(3)*b(3)) .gt. dmin) then
                        i2 = i
                        ino2 = inob
                        noeub = zk8(ilisno+i2-1)
                        goto 260
                    endif
250              continue
!
260              continue
!
! ---              DEFINITION DU VECTEUR N1
! ---              ON ESSAIE D'ABORD N1 = I X B
!
                n1(1) = zero
                n1(2) = -b(3)
                n1(3) = b(2)
!
                if (sqrt(b(3)*b(3)+b(2)*b(2)) .le. dmin) then
!
! ---                ON FAIT UN AUTRE ESSAI AVEC N1 = J X B
!
                    n1(1) = b(3)
                    n1(2) = zero
                    n1(3) = -b(1)
!
                    if (sqrt(b(3)*b(3)+b(1)*b(1)) .le. dmin) then
                        call u2mess('F', 'MODELISA4_41')
                    endif
                endif
!
! ---              DEFINITION DU VECTEUR N2 = B X N1
!
                n2(1) = b(2)*n1(3) - b(3)*n1(2)
                n2(2) = b(3)*n1(1) - b(1)*n1(3)
                n2(3) = b(1)*n1(2) - b(2)*n1(1)
!
! ---              DEFINITION DE ML1
!
                ml1(1,2) = n2(2)*n1(1) - n1(2)*n2(1)
                ml1(1,3) = n2(3)*n1(1) - n1(3)*n2(1)
                ml1(2,1) = n2(1)*n1(2) - n1(1)*n2(2)
                ml1(2,3) = n2(3)*n1(2) - n1(3)*n2(2)
                ml1(3,1) = n2(1)*n1(3) - n1(1)*n2(3)
                ml1(3,2) = n2(2)*n1(3) - n1(2)*n2(3)
                ml1(1,5) = -ml1(1,2)
                ml1(1,6) = -ml1(1,3)
                ml1(2,4) = -ml1(2,1)
                ml1(2,6) = -ml1(2,3)
                ml1(3,4) = -ml1(3,1)
                ml1(3,5) = -ml1(3,2)
!
! ---              ON VERIFIE QUE LES NOEUDS DE LA LISTE SONT
! ---              EFFECTIVEMENT ALIGNES
!
                do 270 i = 2, lonlis
                    call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+i-1) ), inoem)
!
                    m(1) = zr( jcoor-1+3* (inoem-1)+1) - zr(jcoor-1+3* (inoa-1)+1)
                    m(2) = zr( jcoor-1+3* (inoem-1)+2) - zr(jcoor-1+3* (inoa-1)+2)
                    m(3) = zr( jcoor-1+3* (inoem-1)+3) - zr(jcoor-1+3* (inoa-1)+3)
!
!
! ---                 DEFINITION DU VECTEUR ABM = AB X AM
!
                    abm(1) = b(2)*m(3) - b(3)*m(2)
                    abm(2) = b(3)*m(1) - b(1)*m(3)
                    abm(3) = b(1)*m(2) - b(2)*m(1)
!
                    if (sqrt(abm(1)*abm(1)+abm(2)*abm(2)+abm(3)*abm(3) ) .gt. dmin) then
                        call u2mess('F', 'MODELISA4_42')
                    endif
270              continue
!
! ---           PREMIERE RELATION  : (UB-UA).B = 0
!
                nbterm = 6
!
                zk8(jlisno+1-1) = noeub
                zk8(jlisno+2-1) = noeua
                zk8(jlisno+3-1) = noeub
                zk8(jlisno+4-1) = noeua
                zk8(jlisno+5-1) = noeub
                zk8(jlisno+6-1) = noeua
!
                zk8(jlisdl+1-1) = 'DX'
                zk8(jlisdl+2-1) = 'DX'
                zk8(jlisdl+3-1) = 'DY'
                zk8(jlisdl+4-1) = 'DY'
                zk8(jlisdl+5-1) = 'DZ'
                zk8(jlisdl+6-1) = 'DZ'
!
                zr(jliscr+1-1) = b(1)
                zr(jliscr+2-1) = -b(1)
                zr(jliscr+3-1) = b(2)
                zr(jliscr+4-1) = -b(2)
                zr(jliscr+5-1) = b(3)
                zr(jliscr+6-1) = -b(3)
!
                call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8( jlisno), zi(jlisdm),&
                            zr(jlisdi), nbterm, beta, betac, betaf,&
                            typcoe, typval, typlag, 0.d0, lisrel)
!
! ---              CALCUL DU COEFFICIENT K
!
                coek = ( n1(1)*n1(1)+n1(2)*n1(2)+n1(3)*n1(3))* (b(1)*b( 1)+b(2)*b(2)+b(3)*b(3) )
                coek1 = 1.0d0/coek
!
                do 330 i = 2, lonlis
                    call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+i-1) ), inoem)
                    if (inoem .eq. ino2) goto 330
!
                    m(1) = zr( jcoor-1+3* (inoem-1)+1) - zr(jcoor-1+3* (inoa-1)+1)
                    m(2) = zr( jcoor-1+3* (inoem-1)+2) - zr(jcoor-1+3* (inoa-1)+2)
                    m(3) = zr( jcoor-1+3* (inoem-1)+3) - zr(jcoor-1+3* (inoa-1)+3)
!
! ---                 DEFINITION DE ML3
!
                    ml3(1,2) = -m(3)
                    ml3(1,3) = m(2)
                    ml3(2,1) = m(3)
                    ml3(2,3) = -m(1)
                    ml3(3,1) = -m(2)
                    ml3(3,2) = m(1)
!
! ---                 CALCUL DE ML5 <-- ML3.ML1
!
                    call pmppr(ml3, 3, 3, 1, ml1,&
                               3, 9, 1, ml5, 3,&
                               9)
!
                    do 290 j1 = 1, 3
                        do 280 j2 = 1, 9
                            ml5(j1,j2) = coek1*ml5(j1,j2)
280                      continue
290                  continue
!
! ---                 RAJOUT DE ML4
!
                    ml5(1,1) = ml5(1,1) - un
                    ml5(2,2) = ml5(2,2) - un
                    ml5(3,3) = ml5(3,3) - un
                    ml5(1,7) = ml5(1,7) + un
                    ml5(2,8) = ml5(2,8) + un
                    ml5(3,9) = ml5(3,9) + un
!
!
! ---     ECRITURE DES 3 RELATIONS CORRESPONDANTES A LA RELATION
! ---     MATRICIELLE : (ML4+1/K*ML3*ML1)*UABM = 0
!
                    nbterm = 9
!
                    do 300 k = 1, 3
!
                        zk8(jlisno+k-1) = noeua
                        zk8(jlisno+3+k-1) = noeub
                        zk8(jlisno+6+k-1) = zk8(ilisno+i-1)
!
                        zk8(jlisdl+3* (k-1)+1-1) = 'DX'
                        zk8(jlisdl+3* (k-1)+2-1) = 'DY'
                        zk8(jlisdl+3* (k-1)+3-1) = 'DZ'
300                  continue
!
                    do 320 j1 = 1, 3
                        do 310 j2 = 1, 9
                            zr(jliscr+j2-1) = ml5(j1,j2)
310                      continue
!
                        call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                                    zr(jlisdi), nbterm, beta, betac, betaf,&
                                    typcoe, typval, typlag, 0.d0, lisrel)
320                  continue
330              continue
! ---       FIN DU CAS LONLIS GT 2
            endif
! ---     FIN DU CAS OU LES NOEUDS SONT ALIGNES
        endif
! ---   FIN DU CAS OU L'ON NE PEUT PAS TROUVER 3 NOEUDS FORMANT
! ---   UN TRIANGLE
    endif
!
!
!
! --- DESTRUCTION DES OBJETS DE TRAVAIL
    call jedetr('&&DRZ03D.LISNO')
    call jedetr('&&DRZ03D.LISDDL')
    call jedetr('&&DRZ03D.COER')
    call jedetr('&&DRZ03D.COEC')
    call jedetr('&&DRZ03D.DIRECT')
    call jedetr('&&DRZ03D.DIME')
!
    call jedema()
end subroutine
