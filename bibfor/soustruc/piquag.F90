subroutine piquag(epsi, rip, rep, rit, ret,&
                  bet, eso, hso, h2, h3,&
                  l3, l4, l5, l6, tetaf,&
                  xmax, ymax, lmax, nt, mailla,&
                  nogrno, typsou)
! aslint: disable=W1504
    implicit   none
    include 'jeveux.h'
!
    include 'asterc/r8pi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/piqall.h'
    include 'asterfort/piqpiq.h'
    include 'asterfort/piqpla.h'
    include 'asterfort/piqrot.h'
    include 'asterfort/piqsym.h'
    integer :: nt
    real(kind=8) :: epsi, rip, rep, rit, ret, bet, eso, hso, h2, h3, l3, l4, l5
    real(kind=8) :: l6, tetaf, xmax, ymax, lmax
    character(len=8) :: typsou
    character(len=8) :: mailla
    character(len=24) :: nogrno
!-----------------------------------------------------------------------
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
!     OPERATEUR: "DEFI_GROUP" , MOTCLE FACTEUR "EQUE_PIQUA"
!     AUTEUR Y. WADIER
!
!     REALISE LA TRANSFORMATION : GEOMETRIE DE REFERENCE --> PIQUAGE
!
!-----------------------------------------------------------------------
    integer :: ino, nbnoeu, nume, iagrn, jvale
    real(kind=8) :: x0, y0, z0, x1, y1, z1
    real(kind=8) :: xp, yp, zp, x, y, z
    real(kind=8) :: alp, r1, tetax
    logical :: rotati, allong
    logical :: rota, plaq, piqu, allo
    logical :: zone1, zone2, zone3, zone4, zone5, zone6, zone7, zone8
    logical :: quar1, quar2, quar3, quar4
    character(len=8) :: k8b
    character(len=24) :: grpnoe, cooval
!     ------------------------------------------------------------------
!
    call jemarq()
!
!-------------INITIALISATION DES DONNEES LOGIQUES-----------------------
!
!     ROTATI = .TRUE. : ON FAIT LA ROTATION
!     ALLONG = .TRUE. : ON FAIT L'ALLONGEMENT
!
    rotati = .true.
    allong = .true.
!
!     ROND = .TRUE. : ON ECRIT LES COORDONNEES DU PIQUAGE ROND
!     ROTA = .TRUE. : ON ECRIT LES COORDONNEES DU PIQUAGE APRES ROTATION
!     PLAQ = .TRUE. : ON ECRIT LES COORDONNEES DU PIQUAGE SUR PLAQUE
!     PIQU = .TRUE. : ON ECRIT LES COORDONNEES DU PIQUAGE
!     ALLO = .TRUE. : ON ECRIT LES COORDONNEES DU PIQUAGE ALLONGE
!
!      ROND = .TRUE.
    rota = .true.
    plaq = .true.
    piqu = .true.
    allo = .true.
!
    rota = rota .and. rotati
    allo = allo .and. allong
!
    grpnoe = mailla//'.GROUPENO       '
    cooval = mailla//'.COORDO    .VALE'
    call jelira(jexnom(grpnoe, nogrno), 'LONUTI', nbnoeu, k8b)
    call jeveuo(jexnom(grpnoe, nogrno), 'L', iagrn)
    call jeveuo(cooval, 'E', jvale)
!
    do 10 ino = 1, nbnoeu
!
!------------ LECTURE DES COORDONNEES DES NOEUDS DE L'EQUERRE ----------
!
        nume = zi(iagrn+ino-1)
        x0 = zr(jvale-1+3*(nume-1)+1)
        y0 = zr(jvale-1+3*(nume-1)+2)
        z0 = zr(jvale-1+3*(nume-1)+3)
!
!--------------- TRANSFORMATION EQUERRE  --->  PIQUAGE ROND ------------
!
        alp = y0 * r8pi() / ( 2.0d0 * ymax )
        x1 = x0 * cos( alp )
        y1 = x0 * sin( alp )
        z1 = z0
!
!------------------------ ROTATION DU PIQUAGE ROND ---------------------
!
        if (rotati) then
            call piqrot(x1, y1, tetax, nt, ret,&
                        rit, rep, tetaf, epsi)
        endif
!
        x1 = x0 * cos(alp+tetax)
        y1 = x0 * sin(alp+tetax)
        z1 = z0
!
!-------------------------- REPERAGE DES 4 QUARTS ----------------------
!
        quar1 = .false.
        quar2 = .false.
        quar3 = .false.
        quar4 = .false.
!
        if (x1 .ge. 0.d0 .and. y1 .ge. 0.d0) quar1 = .true.
!
        if (x1 .lt. 0.d0 .and. y1 .ge. 0.d0) quar2 = .true.
!
        if (x1 .lt. 0.d0 .and. y1 .lt. 0.d0) quar3 = .true.
!
        if (x1 .ge. 0.d0 .and. y1 .lt. 0.d0) quar4 = .true.
!
!-------------------- RETOUR DANS LE PREMIER QUART  --------------------
!
        call piqsym(x1, y1, quar1, quar2, quar3,&
                    quar4)
!
        r1 = sqrt( x1**2 + y1**2 )
!
!------------------------ DEFINITION DES 8 ZONES -----------------------
!
        zone1 = .false.
        zone2 = .false.
        zone3 = .false.
        zone4 = .false.
        zone5 = .false.
        zone6 = .false.
        zone7 = .false.
        zone8 = .false.
!
        if (h2 .lt. z1) zone1=.true.
        if ((h3.lt.z1) .and. (z1.le.h2)) zone2=.true.
!
        if (((rip-epsi).le.r1) .and. (r1.lt.(l3+epsi))) then
            if ((ret.lt.z1) .and. (z1.le.h3)) zone3=.true.
            if ((0.d0.le.z1) .and. (z1.le.ret)) zone4=.true.
        endif
!
        if (( (l3-epsi).le.r1) .and. (r1.lt.(l4+epsi))) then
            if ((ret.lt.z1) .and. (z1.le.(h3+epsi))) zone5=.true.
            if ((0.d0.le.z1) .and. (z1.le.ret)) zone6=.true.
        endif
!
        if (((l4-epsi).le.r1) .and. (r1.lt.(l5+epsi))) zone7=.true.
        if (l5 .le. r1) zone8=.true.
!
!
!------------ TRANSFORMATION PIQUAGE ROND ---> PIQUAGE CARRE -----------
!
        call piqpla(x1, y1, z1, xp, yp,&
                    zp, zone7, zone8, l4, l6,&
                    epsi)
!
        if (plaq) then
            call piqsym(xp, yp, quar1, quar2, quar3,&
                        quar4)
!
            call piqsym(xp, yp, quar1, quar2, quar3,&
                        quar4)
        endif
!
!------------ TRANSFORMATION PIQUAGE CARRE ---> PIQUAGE ----------------
!
        call piqpiq(xp, yp, zp, x, y,&
                    z, rep, ret, rit, bet,&
                    eso, hso, h2, h3, l4,&
                    l5, zone1, zone2, zone3, zone4,&
                    zone5, zone6, zone7, zone8, typsou)
!
        if (piqu) then
            call piqsym(x, y, quar1, quar2, quar3,&
                        quar4)
!
            call piqsym(x, y, quar1, quar2, quar3,&
                        quar4)
        endif
!
!--------------------- ALLONGEMENT DU PIQUAGE --------------------------
!
        if (allong) call piqall(x, ret, rit, rep, xmax,&
                                lmax, epsi)
!
        if (allo) then
            call piqsym(x, y, quar1, quar2, quar3,&
                        quar4)
!
        endif
!
        zr(jvale-1+3*(nume-1)+1) = x
        zr(jvale-1+3*(nume-1)+2) = y
        zr(jvale-1+3*(nume-1)+3) = z
!
10  end do
!
    call jedema()
!
end subroutine
