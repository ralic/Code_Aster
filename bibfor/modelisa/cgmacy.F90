subroutine cgmacy(mofaz, iocc, nomaz, lismaz, nbma)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_6
!.======================================================================
    implicit none
!
!       CGMACY -- TRAITEMENT DE L'OPTION CYLINDRE
!                 DU MOT FACTEUR CREA_GROUP_MA DE
!                 LA COMMANDE DEFI_GROUP
!
!      CETTE FONCTIONNALITE PERMET DE CREER UN GROUP_MA CONSTITUE
!      DE TOUTES LES MAILLES DONT UN NOEUD AU MOINS APPARTIENT
!      A CYLINDRE DONT L'AXE ET LE RAYON SONT DEFINIS PAR
!      L'UTILISATEUR.
!
! -------------------------------------------------------
!  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_MA'
!  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
!  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
!  LISMAZ        - JXVAR - K24  - : NOM DE LA LISTE DE MAILLES DONT UN
!                                   NOEUD AU MOINS APPARTIENT AU
!                                   CYLINDRE DEFINI PAR L'UTILISATEUR
!  NBMA          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
! -------------------------------------------------------
!
!.========================= DEBUT DES DECLARATIONS ====================
    include 'jeveux.h'
!
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterc/r8dgrd.h'
    include 'asterc/r8prem.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utcono.h'
    include 'asterfort/wkvect.h'
!
! -----  ARGUMENTS
    character(len=*) :: mofaz, nomaz, lismaz
!
! --------- VARIABLES LOCALES ---------------------------
    character(len=1) :: k1bid
    character(len=8) :: noma, k8bid, nomail
    character(len=16) :: motfac, mocle(3)
    character(len=24) :: lismai
    character(len=16) :: selec
!
!
    real(kind=8) :: x0(3), x(3), xx0(3), axe(3), angle(2)
    integer :: iarg
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
!-----------------------------------------------------------------------
    integer :: ibid, idcoor, idlima, idnoeu, ier, ima, ino
    integer :: iocc, iret, nangle, nb, nbma, nbmai, nbno
    integer :: nbnod, ndim, nrayon, numnoe, nv, nvect
    real(kind=8) :: ang, d2, eps, psca, r8bid
    real(kind=8) :: rayon, un, xnorm, xnorm2, xnoxx0, xnoxx2, zero
!
!-----------------------------------------------------------------------
    call jemarq()
!
! --- INITIALISATIONS :
!     ---------------
    motfac = mofaz
    noma = nomaz
    lismai = lismaz
!
! --- RECUPERATION DU TYPE DE VERIFICATION A APPLIQUER :
!     --------------------------------------------------
    call getvtx(motfac, 'CRIT_NOEUD', iocc, iarg, 1,&
                selec, ibid)
!
    zero = 0.0d0
    un = 1.0d0
!
    x0(1) = zero
    x0(2) = zero
    x0(3) = zero
!
    x(1) = zero
    x(2) = zero
    x(3) = zero
!
    xx0(1) = zero
    xx0(2) = zero
    xx0(3) = zero
!
    axe(1) = zero
    axe(2) = zero
    axe(3) = zero
!
    rayon = zero
!
    eps = 100.0d0*r8prem()
!
    nbma = 0
!
! --- RECUPERATION DE LA DIMENSION DU MAILLAGE :
!     ----------------------------------------
    call dismoi('F', 'Z_CST', noma, 'MAILLAGE', ndim,&
                k8bid, ier)
    if (k8bid(1:3) .eq. 'OUI') then
        ndim = 2
    else
        ndim = 3
    endif
!
    if (ndim .ne. 3) then
        call u2mess('F', 'MODELISA3_73')
    endif
!
! --- RECUPERATION DES COORDONNES DES NOEUDS DU MAILLAGE :
!     --------------------------------------------------
    call jeveuo(noma//'.COORDO    .VALE', 'L', idcoor)
!
! --- RECUPERATION DU POINT SITUE SUR L'AXE DU CYLINDRE :
!     -------------------------------------------------
    mocle(1) = 'POINT'
    mocle(2) = 'NOEUD_CENTRE'
    mocle(3) = 'GROUP_NO_CENTRE'
    call utcono(motfac, mocle, iocc, noma, ndim,&
                x0, iret)
!
! --- RECUPERATION DU RAYON DU CYLINDRE :
!     ---------------------------------
    call getvr8(motfac, 'RAYON', iocc, iarg, 0,&
                rayon, nrayon)
    if (nrayon .eq. 0) then
        call u2mess('F', 'MODELISA3_74')
    else
        call getvr8(motfac, 'RAYON', iocc, iarg, 1,&
                    rayon, nb)
        if (rayon .le. zero) then
            call u2mess('F', 'MODELISA3_75')
        endif
    endif
!
! --- RECUPERATION DE LA DIRECTION DEFINISSANT L'AXE DU CYLINDRE :
!     ----------------------------------------------------------
    call getvr8(motfac, 'ANGL_NAUT', iocc, iarg, 0,&
                r8bid, nangle)
    if (nangle .eq. 0) then
        call getvr8(motfac, 'VECT_NORMALE', iocc, iarg, 0,&
                    r8bid, nvect)
        if (nvect .eq. 0) then
            call u2mess('F', 'MODELISA3_76')
        else
            nvect = -nvect
            if (nvect .ne. 3) then
                call u2mess('F', 'MODELISA3_77')
            else
                call getvr8(motfac, 'VECT_NORMALE', iocc, iarg, nvect,&
                            axe, nv)
            endif
        endif
    else
        nangle = -nangle
        if (nangle .ne. 2) then
            call u2mess('F', 'MODELISA3_78')
        endif
        call getvr8(motfac, 'ANGL_NAUT', iocc, iarg, nangle,&
                    angle, nv)
!
        angle(1) = angle(1)*r8dgrd()
        angle(2) = angle(2)*r8dgrd()
!
        axe(1) = cos(angle(1))*cos(angle(2))
        axe(2) = sin(angle(1))*cos(angle(2))
        axe(3) = -sin(angle(2))
    endif
!
    xnorm2 = axe(1)*axe(1) + axe(2)*axe(2) + axe(3)*axe(3)
!
    if (xnorm2 .eq. zero) then
        call u2mess('F', 'MODELISA3_79')
    endif
!
    xnorm = sqrt(xnorm2)
!
    axe(1) = axe(1)/xnorm
    axe(2) = axe(2)/xnorm
    axe(3) = axe(3)/xnorm
!
! --- RECUPERATION DU NOMBRE DE MAILLES DU MAILLAGE :
!     ---------------------------------------------
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbmai,&
                k8bid, ier)
!
! --- ALLOCATION DU VECTEUR DES NOMS DES MAILLES APPARTENANT AU
! --- CYLINDRE :
!     --------
    call wkvect(lismai, 'V V I', nbmai, idlima)
!
! --- PARCOURS DES MAILLES DU MAILLAGE :
!     --------------------------------
    do 10 ima = 1, nbmai
!
! ---     RECUPERATION DU NOM DE LA MAILLE :
!         --------------------------------
        call jenuno(jexnum(noma//'.NOMMAI', ima), nomail)
!
! ---     RECUPERATION DES CONNECTIVITES DE LA MAILLE :
!         -------------------------------------------
        call jenonu(jexnom(noma//'.NOMMAI', nomail), ibid)
        call jeveuo(jexnum(noma//'.CONNEX', ibid), 'L', idnoeu)
!
! ---     RECUPERATION DU NOMBRE DE CONNECTIVITES DE LA MAILLE :
!         ----------------------------------------------------
        call jenonu(jexnom(noma//'.NOMMAI', nomail), ibid)
        call jelira(jexnum(noma//'.CONNEX', ibid), 'LONMAX', nbno, k1bid)
!
! ---      COMPTE NOMBRE DES NOEUDS D'UN MAILLE DANS LE CYLINDRE :
!          ------------------------------------------------------
        nbnod = 0
!
! ---     BOUCLE SUR LES CONNECTIVITES DE LA MAILLE :
!         -----------------------------------------
        do 20 ino = 1, nbno
!
! ---        NUMERO DU NOEUD :
!            ---------------
            numnoe = zi(idnoeu+ino-1)
!
! ---        COORDONNEES DU NOEUD :
!            --------------------
            x(1) = zr(idcoor-1+3*(numnoe-1)+1)
            x(2) = zr(idcoor-1+3*(numnoe-1)+2)
            x(3) = zr(idcoor-1+3*(numnoe-1)+3)
!
            xx0(1) = x(1) - x0(1)
            xx0(2) = x(2) - x0(2)
            xx0(3) = x(3) - x0(3)
!
            xnoxx2 = xx0(1)*xx0(1) + xx0(2)*xx0(2) + xx0(3)*xx0(3)
!
! ---       SI LE NOEUD COURANT EST CONFONDU AVEC LE NOEUD DE
! ---       L'AXE DU CYLINDRE, ON AFFECTE  LA MAILLE COURANTE A LA
! ---       LISTE DE MAILLES QUI SERA AFFECTEE AU GROUP_MA :
!           ----------------------------------------------
            if (xnoxx2 .eq. zero) then
                nbma = nbma + 1
                zi(idlima+nbma-1) = ima
                goto 10
            else
!
                xnoxx0 = sqrt (xnoxx2)
!
                xx0(1) = xx0(1)/xnoxx0
                xx0(2) = xx0(2)/xnoxx0
                xx0(3) = xx0(3)/xnoxx0
!
! ---         CALCUL DE L'ANGLE FORME PAR L'AXE DU CYLINDRE
! ---         AVEC LE VECTEUR POSITION COURANT XX0 :
!             ------------------------------------
                psca = abs(xx0(1)*axe(1) + xx0(2)*axe(2) + xx0(3)*axe( 3))
                if (psca .gt. un) then
                    psca = psca - eps
                endif
                ang = acos(psca)
!
! ---         CALCUL DE LA DISTANCE DU NOEUD COURANT A L'AXE
! ---         DU CYLINDRE :
!             -----------
                d2 = (&
                     (&
                     x(1)-x0(1))*(x(1)-x0(1)) + (x(2)-x0(2))*(x(2)- x0(2)) + (x(3)-x0(3))*(x(3)-x&
                     &0(3)))*sin(ang&
                     )*sin(ang&
                     )
!
! ---      SI LE MOT CLE SIMPLE CRIT_NOEUD EST EGAL A AU MOINS UN NOEUD
!          -------------------------------------------------------------
!
                if (selec .eq. 'AU_MOINS_UN') then
!
! ---         SI LE NOEUD COURANT EST DANS LE CYLINDRE, ON AFFECTE
! ---         LA MAILLE COURANTE A LA LISTE DE MAILLES QUI SERA
! ---         AFFECTEE AU GROUP_MA :
!             --------------------
                    if (d2 .le. rayon*rayon) then
                        nbma = nbma + 1
                        zi(idlima+nbma-1) = ima
                        goto 10
                    endif
!
! ---         SI LE MOT CLE SIMPLE CRIT_NOEUD EST EGAL A TOUT OU
! ---         MAJORITE, COMPTER LE NOMBRE DES NOEUDS D'UNE MAILLE
! ---         DANS LE CYLINDRE :
!             ------------------------------------------------
                    else if((selec.eq.'TOUS').or. (selec.eq.'MAJORITE'))&
                then
!
                    if (d2 .le. rayon*rayon) then
                        nbnod=nbnod+1
                    endif
!
                endif
!
            endif
!
20      continue
!
        if (selec .eq. 'TOUS') then
            if (nbnod .eq. nbno) then
                nbma = nbma + 1
                zi(idlima+nbma-1) = ima
                goto 10
            endif
        endif
!
        if (selec .eq. 'MAJORITE') then
            if (nbnod .ge. (nbno+1)/2) then
                nbma = nbma + 1
                zi(idlima+nbma-1) = ima
                goto 10
            endif
        endif
!
10  end do
!
    call jedema()
!.============================ FIN DE LA ROUTINE ======================
end subroutine
