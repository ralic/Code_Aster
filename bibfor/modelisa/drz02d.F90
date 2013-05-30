subroutine drz02d(lisnoz, lonlis, chargz, typlaz, lisrez,&
                  dmin)
!
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
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: charge
    character(len=19) :: lisrel
    character(len=24) :: lisnoe
    character(len=*) :: chargz, lisnoz, typlaz, lisrez
    real(kind=8) :: dmin
! --------------------------------------------------------------
!     BLOCAGE DES DEPLACEMENTS RELATIFS D'UNE LISTE DE NOEUDS
!     SPECIFIEE PAR L'UTILISATEUR DANS LE CAS OU L'ON EST
!     EN 2D ET AUCUN  NOEUD NE PORTE LE DDL DRZ
! --------------------------------------------------------------
!  LISNOE - IN    - K24 - : NOM DE LA LISTE DES
!         -       -     -   NOEUDS A LIER
! --------------------------------------------------------------
!  LONLIS - IN    - I   - : LONGUEUR DE LA LISTE DES
!         -       -     -   NOEUDS A LIER
! --------------------------------------------------------------
!  CHARGE - IN    - K8  - : NOM DE LA SD CHARGE
!         - JXIN  -     -
! --------------------------------------------------------------
! TYPLAG  - IN    - K2  - : TYPE DES MULTIPLICATEURS DE LAGRANGE
!                           ASSOCIES A LA RELATION :
!                       SI = '12'  LE PREMIER LAGRANGE EST AVANT
!                                  LE NOEUD PHYSIQUE
!                                  LE SECOND LAGRANGE EST APRES
!                       SI = '22'  LE PREMIER LAGRANGE EST APRES
!                                  LE NOEUD PHYSIQUE
!                                  LE SECOND LAGRANGE EST APRES
! --------------------------------------------------------------
!  LISREL - IN    - K19 - : NOM DE LA SD
!         - JXVAR -     -   LISTE DE RELATIONS
! --------------------------------------------------------------
!  DMIN   - IN    - R8 - : LONGUEUR EN DESSOUS DE LAQUELLE ON CONSIDERE
!                          QUE 2 POINTS SONT CONFONDUS
! --------------------------------------------------------------
!
!
! --------- VARIABLES LOCALES ---------------------------
!-----------------------------------------------------------------------
    integer :: i, i2, ibid, ier, ierd, ilisno, in
    integer :: ino1, ino2, inom, j, jcoor, jliscc, jliscr
    integer :: jlisdi, jlisdl, jlisdm, jlisno, jnoma, lonlis, memnoe
    integer :: nbcmp, nbec, nbterm, nddla, nmocl
    real(kind=8) :: beta, d2, d21, un, x, x0, y
    real(kind=8) :: y0
!-----------------------------------------------------------------------
    parameter (nmocl=300)
    complex(kind=8) :: betac
    character(len=2) :: typlag
    real(kind=8) :: lab
    character(len=4) :: typval, typcoe
    character(len=8) :: betaf, resu
    character(len=8) :: mod, nomg, k8bid
    character(len=8) :: noma, nomcmp(nmocl)
    character(len=9) :: nomte
    character(len=16) :: type, oper
    character(len=19) :: ligrmo
    integer :: ntypel(nmocl)
    integer :: vali(2)
    character(len=1) :: k1bid
! --------- FIN  DECLARATIONS  VARIABLES LOCALES --------
    call jemarq()
    call getres(resu, type, oper)
    lisrel = lisrez
    charge = chargz
    typlag = typlaz
    lisnoe = lisnoz
!
    betaf = '&FOZERO'
    beta = 0.0d0
    betac = (0.0d0,0.0d0)
    un = 1.0d0
    call assert(dmin .gt. 0.d0)
!
    call dismoi('F', 'NOM_MODELE', charge(1:8), 'CHARGE', ibid,&
                mod, ier)
    ligrmo = mod(1:8)//'.MODELE'
    call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
    noma = zk8(jnoma)
    typcoe = 'REEL'
!
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
    call wkvect('&&DRZ02D.LISNO', 'V V K8', nbterm, jlisno)
! ---  VECTEUR DU NOM DES DDLS
    call wkvect('&&DRZ02D.LISDDL', 'V V K8', nbterm, jlisdl)
! ---  VECTEUR DES COEFFICIENTS REELS
    call wkvect('&&DRZ02D.COER', 'V V R', nbterm, jliscr)
! ---  VECTEUR DES COEFFICIENTS COMPLEXES
    call wkvect('&&DRZ02D.COEC', 'V V C', nbterm, jliscc)
! ---  VECTEUR DES DIRECTIONS DES DDLS A CONTRAINDRE
    call wkvect('&&DRZ02D.DIRECT', 'V V R', 3*nbterm, jlisdi)
! ---  VECTEUR DES DIMENSIONS DE CES DIRECTIONS
    call wkvect('&&DRZ02D.DIME', 'V V I', nbterm, jlisdm)
!
! --- RECUPERATION DU TABLEAU DES COORDONNEES
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
! --- ACQUISITION DE LA LISTE DES NOEUDS A LIER
! --- (CETTE LISTE EST NON REDONDANTE)
    call jeveuo(lisnoe, 'L', ilisno)
!
    call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+1-1)), ino1)
    memnoe = 0
!
!
!
!     1) CAS OU TOUS LES NOEUDS SONT CONFONDUS :
!        ATTENTION : ON CALCULE AUSSI INO2,I2,X0 ET Y0 POUR LE CAS 2)
!     ----------------------------------------------------------------
    do 20 i = 2, lonlis
        call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+i-1)), in)
        x = zr(jcoor-1+3* (in-1)+1) - zr(jcoor-1+3* (ino1-1)+1)
        y = zr(jcoor-1+3* (in-1)+2) - zr(jcoor-1+3* (ino1-1)+2)
!
        ino2 = in
        i2 = i
        x0 = x
        y0 = y
!
        lab=sqrt(x*x+y*y)
        if (abs(lab) .gt. dmin) goto 40
20  end do
!
    nbterm = 2
!
    zk8(jlisno+2-1) = zk8(ilisno+1-1)
    zr(jliscr+1-1) = un
    zr(jliscr+2-1) = -un
!
    do 30 i = 2, lonlis
        zk8(jlisno+1-1) = zk8(ilisno+i-1)
! ---  PREMIERE RELATION
! ---  DX(M) -DX(A) = 0
        zk8(jlisdl+1-1) = 'DX'
        zk8(jlisdl+2-1) = 'DX'
        call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                    zr(jlisdi), nbterm, beta, betac, betaf,&
                    typcoe, typval, typlag, 0.d0, lisrel)
!
! ---  DEUXIEME RELATION
! ---  DY(M) -DY(A) = 0
        zk8(jlisdl+1-1) = 'DY'
        zk8(jlisdl+2-1) = 'DY'
        call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                    zr(jlisdi), nbterm, beta, betac, betaf,&
                    typcoe, typval, typlag, 0.d0, lisrel)
30  end do
    memnoe = 1
!
!
!
!
!     2) CAS OU LES NOEUDS NE SONT PAS TOUS CONFONDUS :
!      ---------------------------------------------------------------
40  continue
    if (memnoe .eq. 0) then
!
! ---   LES NOEUDS D'INDICE 1 ET I2 DE LA LISTE ONT DES
! ---   COORDONNEES DIFFERENTES
!
        do 50 j = 2, lonlis
            call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+j-1)), in)
!
            if (in .eq. ino2) goto 50
!
            x = zr(jcoor-1+3* (in-1)+1) - zr(jcoor-1+3* (ino1-1)+1)
            y = zr(jcoor-1+3* (in-1)+2) - zr(jcoor-1+3* (ino1-1)+2)
!
! ---   PREMIERE RELATION
! ---   DX(M) + DX(A)*(-1+Y0*Y/(X0**2+Y0**2)) - DX(B)*Y*Y0/(X0**2+Y0**2)
! --- + DY(B)*Y*X0/(X0**2+Y0**2) - DY(A)*Y*X0/(X0**2+Y0**2) = 0
!
            nbterm = 5
!
            zk8(jlisno+1-1) = zk8(ilisno+j-1)
            zk8(jlisno+2-1) = zk8(ilisno+1-1)
            zk8(jlisno+3-1) = zk8(ilisno+i2-1)
            zk8(jlisno+4-1) = zk8(ilisno+1-1)
            zk8(jlisno+5-1) = zk8(ilisno+i2-1)
!
            zk8(jlisdl+1-1) = 'DX'
            zk8(jlisdl+2-1) = 'DX'
            zk8(jlisdl+3-1) = 'DX'
            zk8(jlisdl+4-1) = 'DY'
            zk8(jlisdl+5-1) = 'DY'
!
            d2 = x0*x0 + y0*y0
            d21 = 1.0d0/d2
!
            zr(jliscr+1-1) = un
            zr(jliscr+2-1) = -un + y0*y*d21
            zr(jliscr+3-1) = -y0*y*d21
            zr(jliscr+4-1) = -x0*y*d21
            zr(jliscr+5-1) = x0*y*d21
!
            call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                        zr(jlisdi), nbterm, beta, betac, betaf,&
                        typcoe, typval, typlag, 0.d0, lisrel)
!
! ---   DEUXIEME RELATION
! ---   DY(M) + DY(A)*(-1+X0*X/(X0**2+Y0**2)) + DX(B)*X*Y0/(X0**2+Y0**2)
! --- - DX(A)*Y0*X/(X0**2+Y0**2) - DY(B)*X*X0/(X0**2+Y0**2) = 0
!
            nbterm = 5
!
            zk8(jlisno+1-1) = zk8(ilisno+j-1)
            zk8(jlisno+2-1) = zk8(ilisno+1-1)
            zk8(jlisno+3-1) = zk8(ilisno+i2-1)
            zk8(jlisno+4-1) = zk8(ilisno+1-1)
            zk8(jlisno+5-1) = zk8(ilisno+i2-1)
!
            zk8(jlisdl+1-1) = 'DY'
            zk8(jlisdl+2-1) = 'DY'
            zk8(jlisdl+3-1) = 'DX'
            zk8(jlisdl+4-1) = 'DX'
            zk8(jlisdl+5-1) = 'DY'
!
            zr(jliscr+1-1) = un
            zr(jliscr+2-1) = -un + x0*x*d21
            zr(jliscr+3-1) = y0*x*d21
            zr(jliscr+4-1) = -y0*x*d21
            zr(jliscr+5-1) = -x0*x*d21
!
            call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                        zr(jlisdi), nbterm, beta, betac, betaf,&
                        typcoe, typval, typlag, 0.d0, lisrel)
50      continue
!
! ---  TROISIEME RELATION
! ---  -DX(A)*X0 - DY(A)*Y0 + DX(B)*X0 + DY(B)*Y0 = 0
!
        nbterm = 4
!
        zk8(jlisno+1-1) = zk8(ilisno+1-1)
        zk8(jlisno+2-1) = zk8(ilisno+1-1)
        zk8(jlisno+3-1) = zk8(ilisno+i2-1)
        zk8(jlisno+4-1) = zk8(ilisno+i2-1)
!
        zk8(jlisdl+1-1) = 'DX'
        zk8(jlisdl+2-1) = 'DY'
        zk8(jlisdl+3-1) = 'DX'
        zk8(jlisdl+4-1) = 'DY'
!
        zr(jliscr+1-1) = -x0
        zr(jliscr+2-1) = -y0
        zr(jliscr+3-1) = x0
        zr(jliscr+4-1) = y0
!
        call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                    zr(jlisdi), nbterm, beta, betac, betaf,&
                    typcoe, typval, typlag, 0.d0, lisrel)
!
! ---   FIN DU CAS 2D SANS DDL DE ROTATION OU TOUS LES
! ---   NOEUDS NE SONT PAS CONFONDUS
    endif
!
! --- DESTRUCTION DES OBJETS DE TRAVAIL
!
    call jedetr('&&DRZ02D.LISNO')
    call jedetr('&&DRZ02D.LISDDL')
    call jedetr('&&DRZ02D.COER')
    call jedetr('&&DRZ02D.COEC')
    call jedetr('&&DRZ02D.DIRECT')
    call jedetr('&&DRZ02D.DIME')
!
    call jedema()
end subroutine
