subroutine cgmafn(mofaz, iocc, nomaz, lismaz, nbma)
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
!.======================================================================
! aslint: disable=
    implicit none
!
!       CGMAFN -- TRAITEMENT DE L'OPTION FACE_NORMALE
!                 DU MOT FACTEUR CREA_GROUP_MA DE
!                 LA COMMANDE DEFI_GROUP
!
!      CETTE FONCTIONNALITE PERMET DE CREER UN GROUP_MA CONSTITUE
!      DE TOUTES LES MAILLES 'SURFACIQUES' DONT LA NORMALE
!      CALCULEE A PARTIR DES 3 PREMIERS NOEUDS DE L'ELEMENT
!      PAR N = 12 X 13 EST PARALLELE AU VECTEUR DEFINI PAR
!      L'UTILISATEUR PAR LES MOTS CLES ANGL_NAUT OU VECT_NORMALE.
!      ON DIRA QUE LES 2 VECTEURS SONT PARALLELES SI L'ANGLE
!      FORME PAR CES 2 VECTEURS EST INFERIEUR A LA VALEUR
!      DONNEE PAR L'UTILISATEUR APRES LE MOT CLE ANGL_PREC.
!      LA VALEUR PAR DEFAUT DE CET ANGLE EST EGALE A 0.5 DEGRE.
!
! -------------------------------------------------------
!  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_MA'
!  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
!  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
!  LISMAZ        - JXVAR - K24  - : NOM DE LA LISTE DE MAILLES
!                                   SURFACIQUES DE NORMALE PARALLELE AU
!                                   VECTEUR DEFINI PAR L'UTILISATEUR
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
    include 'asterfort/canor2.h'
    include 'asterfort/canor3.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
!
! -----  ARGUMENTS
    character(len=*) :: mofaz, nomaz, lismaz
!
! --------- VARIABLES LOCALES ---------------------------
    character(len=1) :: k1bid
    character(len=4) :: cdim
    character(len=8) :: noma, k8bid, nomail, nomtyp, ouinon
    character(len=16) :: motfac
    character(len=24) :: lismai
    character(len=24) :: valk
!
    real(kind=8) :: angle(3), vecnor(3), coor(3, 9)
    integer :: vali(3)
    integer :: iarg
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
!-----------------------------------------------------------------------
    integer :: iatyma, ibid, idcoor, idlima, idnoeu, ier, ima
    integer :: ino1, ino2, ino3, iocc, ityp, jtyp, nangle
    integer :: nb, nbang, nbma, nbmai, nbno, nbo, nboui
    integer :: ndim, ndim1, nv, nvect
    real(kind=8) :: a, ang, angpre, b, c, eps, psca
    real(kind=8) :: r8bid, un, undemi, xnorel, xnorm
    real(kind=8) :: xnorm2, zero
!-----------------------------------------------------------------------
    call jemarq()
!
! --- INITIALISATIONS :
!     ---------------
    motfac = mofaz
    noma = nomaz
    lismai = lismaz
!
    zero = 0.0d0
    undemi = 0.5d0
    un = 1.0d0
!
    angle(1) = zero
    angle(2) = zero
    angle(3) = zero
!
    vecnor(1) = zero
    vecnor(2) = zero
    vecnor(3) = zero
!
    a = zero
    b = zero
    c = zero
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
! --- RECUPERATION DE LA DIRECTION FOURNIE PAR L'UTILISATEUR
! --- ET COINCIDANT AVEC LA NORMALE DES ELEMENTS SURFACIQUES
! --- QUE L'ON SOUHAITE RECUPERER :
!     ---------------------------
    call getvr8(motfac, 'ANGL_NAUT', iocc, iarg, 0,&
                r8bid, nangle)
    if (nangle .eq. 0) then
        call getvr8(motfac, 'VECT_NORMALE', iocc, iarg, 0,&
                    r8bid, nvect)
        if (nvect .eq. 0) then
            call u2mess('F', 'MODELISA3_80')
        else
            nvect = -nvect
            nvect = min (nvect,ndim)
            call getvr8(motfac, 'VECT_NORMALE', iocc, iarg, nvect,&
                        vecnor, nv)
            if (abs(nv) .ne. ndim) then
                valk = motfac
                vali (1) = iocc
                call u2mesg('F+', 'MODELISA9_36', 1, valk, 1,&
                            vali, 0, 0.d0)
                if (ndim .eq. 2) then
                    call u2mesg('F+', 'MODELISA9_24', 0, ' ', 0,&
                                0, 0, 0.d0)
                else
                    call u2mesg('F+', 'MODELISA9_25', 0, ' ', 0,&
                                0, 0, 0.d0)
                endif
                vali (1) = abs(nv)
                vali (2) = ndim
                valk = 'VECT_NORMALE'
                call u2mesg('F', 'MODELISA9_39', 1, valk, 2,&
                            vali, 0, 0.d0)
            endif
        endif
    else
        nangle = -nangle
        ndim1 = ndim - 1
        nangle = min (nangle,ndim1)
        call getvr8(motfac, 'ANGL_NAUT', iocc, iarg, nangle,&
                    angle, nv)
        if (abs(nv) .ne. ndim1) then
            valk = motfac
            vali (1) = iocc
            call u2mesg('F+', 'MODELISA9_40', 1, valk, 1,&
                        vali, 0, 0.d0)
            if (ndim .eq. 2) then
                call u2mesg('F+', 'MODELISA9_24', 0, ' ', 0,&
                            0, 0, 0.d0)
            else
                call u2mesg('F+', 'MODELISA9_25', 0, ' ', 0,&
                            0, 0, 0.d0)
            endif
            vali (1) = abs(nv)
            vali (2) = ndim1
            valk = 'ANGL_NAUT'
            call u2mesg('F', 'MODELISA9_43', 1, valk, 2,&
                        vali, 0, 0.d0)
        endif
!
        if (ndim .eq. 2) then
            angle(1) = angle(1)*r8dgrd()
!
            vecnor(1) = cos(angle(1))
            vecnor(2) = sin(angle(1))
            vecnor(3) = zero
        else if (ndim.eq.3) then
            angle(1) = angle(1)*r8dgrd()
            angle(2) = angle(2)*r8dgrd()
!
            vecnor(1) = cos(angle(1))*cos(angle(2))
            vecnor(2) = sin(angle(1))*cos(angle(2))
            vecnor(3) = -sin(angle(2))
        endif
    endif
!
    xnorm2 = vecnor(1)*vecnor(1) + vecnor(2)*vecnor(2) + vecnor(3)*vecnor(3)
!
    if (xnorm2 .eq. zero) then
        call u2mess('F', 'MODELISA3_81')
    endif
!
    xnorm = sqrt(xnorm2)
!
    vecnor(1) = vecnor(1)/xnorm
    vecnor(2) = vecnor(2)/xnorm
    vecnor(3) = vecnor(3)/xnorm
!
! --- RECUPERATION DE L'ANGLE MAX TOLERE ENTRE LA DIRECTION
! --- FOURNIE PAR L'UTILISATEUR ET LA DIRECTION NORMALE A
! --- L'ELEMENT :
!     ---------
    call getvr8(motfac, 'ANGL_PREC', iocc, iarg, 0,&
                angpre, nbang)
    if (nbang .eq. 0) then
        angpre = undemi*r8dgrd()
    else
        call getvr8(motfac, 'ANGL_PREC', iocc, iarg, 1,&
                    angpre, nb)
        angpre = angpre*r8dgrd()
    endif
!
! --- ON REGARDE SI L'ON TIENT COMPTE OU NON DU FAIT QUE LA NORMALE
! --- FOURNIE PAR L'UTILISATEUR ET LA DIRECTION NORMALE A
! --- L'ELEMENT ONT LA MEME ORIENTATION :
!     ---------------------------------
    call getvtx(motfac, 'VERI_SIGNE', iocc, iarg, 0,&
                ouinon, nboui)
    if (nboui .eq. 0) then
        ouinon = 'OUI'
    else
        call getvtx(motfac, 'VERI_SIGNE', iocc, iarg, 1,&
                    ouinon, nbo)
    endif
!
! --- RECUPERATION DE LA DIMENSION DE L'ESPACE DES COORDONNEES :
!     --------------------------------------------------------
    call jelira(noma//'.COORDO    .VALE', 'DOCU', ibid, cdim)
!
    if (cdim .eq. '2   ') then
        ndim=2
    else if (cdim.eq.'3   ') then
        ndim=3
    endif
!
! --- RECUPERATION DU NOMBRE DE MAILLES DU MAILLAGE :
!     ---------------------------------------------
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbmai,&
                k8bid, ier)
!
! --- ALLOCATION DU VECTEUR DES NOMS DES MAILLES DE SURFACE DE
! --- NORMALE PARALLELE AU VECTEUR VECNOR :
!     -----------------------------------
    call wkvect(lismai, 'V V I', nbmai, idlima)
!
! --- RECUPERATION DES COORDONNES DES NOEUDS DU MAILLAGE :
!     --------------------------------------------------
    call jeveuo(noma//'.COORDO    .VALE', 'L', idcoor)
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
! ---     RECUPERATION DU TYPE DE LA MAILLE :
!         ---------------------------------
        call jenonu(jexnom(noma//'.NOMMAI', nomail), ibid)
        call jeveuo(noma//'.TYPMAIL', 'L', iatyma)
        jtyp=iatyma-1+ibid
        ityp = zi(jtyp)
        call jenuno(jexnum('&CATA.TM.NOMTM', ityp), nomtyp)
!
! ---     CAS DU 2D
! ---     LES MAILLES SURFACIQUES SONT DES SEG2 OU DES SEG3 :
!         -------------------------------------------------
        if (ndim .eq. 2 .and. nomtyp(1:3) .eq. 'SEG') then
            ino1 = zi(idnoeu+1-1)
            ino2 = zi(idnoeu+2-1)
!
            coor(1,1)=zr(idcoor-1+3*(ino1-1)+1)
            coor(2,1)=zr(idcoor-1+3*(ino1-1)+2)
            coor(1,2)=zr(idcoor-1+3*(ino2-1)+1)
            coor(2,2)=zr(idcoor-1+3*(ino2-1)+2)
!
! ---         CALCUL DES COMPOSANTES A ET B DU VECTEUR NORMAL
! ---         A L'ELEMENT :
!             -----------
            call canor2(coor, a, b)
!
! ---     CAS DU 3D
! ---     LES MAILLES SURFACIQUES SONT DES TRIA3 OU DES TRIA6
! ---     OU DES TRIA9 OU DES QUAD4 OU DES QUAD8 :
!         --------------------------------------
            elseif (ndim.eq.3.and. (nomtyp(1:4).eq.'TRIA'.or.nomtyp(1:4)&
        .eq.'QUAD')) then
            ino1 = zi(idnoeu+1-1)
            ino2 = zi(idnoeu+2-1)
            ino3 = zi(idnoeu+3-1)
!
            coor(1,1)=zr(idcoor-1+3*(ino1-1)+1)
            coor(2,1)=zr(idcoor-1+3*(ino1-1)+2)
            coor(3,1)=zr(idcoor-1+3*(ino1-1)+3)
!
            coor(1,2)=zr(idcoor-1+3*(ino2-1)+1)
            coor(2,2)=zr(idcoor-1+3*(ino2-1)+2)
            coor(3,2)=zr(idcoor-1+3*(ino2-1)+3)
!
            coor(1,3)=zr(idcoor-1+3*(ino3-1)+1)
            coor(2,3)=zr(idcoor-1+3*(ino3-1)+2)
            coor(3,3)=zr(idcoor-1+3*(ino3-1)+3)
!
! ---         CALCUL DES COMPOSANTES A, B ET C DU VECTEUR NORMAL
! ---         A L'ELEMENT :
!             -----------
            call canor3(coor, a, b, c)
!
! ---     LA MAILLE N'EST PAS DU TYPE SOUHAITE :
!         ------------------------------------
        else
            goto 10
        endif
!
! ---     CALCUL DE L'ANGLE FORME PAR LE VECTEUR NORMAL A L'ELEMENT
! ---     ET LA DIRECTION FOURNIE PAR L'UTILISATEUR :
!         -----------------------------------------
        xnorel = sqrt(a*a + b*b + c*c)
!
! ---     CAS OU L'ON TIENT COMPTE  DU FAIT QUE LA NORMALE FOURNIE
! ---     PAR L'UTILISATEUR ET LA DIRECTION NORMALE A L'ELEMENT
! ---     DOIVENT AVOIR LA MEME ORIENTATION :
!         ---------------------------------
        if (ouinon(1:3) .eq. 'OUI') then
            psca = a*vecnor(1) + b*vecnor(2) + c*vecnor(3)
            if (psca .le. zero) goto 10
        endif
!
        psca = abs(a*vecnor(1) + b*vecnor(2) + c*vecnor(3))/xnorel
        if (psca .gt. un) then
            psca = psca - eps
        endif
        ang = acos(psca)
!
! ---       SI LE VECTEUR NORMAL A L'ELEMENT ET LA DIRECTION FOURNIE
! ---       PAR L'UTILISATEUR  SONT PARALLELES, ON AFFECTE LA MAILLE
! ---       COURANTE A LA LISTE DE MAILLES QUI SERA AFFECTEE AU
! ---       GROUP_MA :
!           --------
        if (abs(ang) .lt. abs(angpre)) then
            nbma = nbma + 1
            zi(idlima+nbma-1) = ima
        endif
10  end do
!
    call jedema()
!.============================ FIN DE LA ROUTINE ======================
end subroutine
