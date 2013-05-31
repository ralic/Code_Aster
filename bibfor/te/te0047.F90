subroutine te0047(optioz, nomtez)
! aslint: disable=W1501
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/diarme.h'
    include 'asterfort/dichoc.h'
    include 'asterfort/dicorn.h'
    include 'asterfort/dicrgr.h'
    include 'asterfort/digouj.h'
    include 'asterfort/dinon2.h'
    include 'asterfort/dinon3.h'
    include 'asterfort/dinon4.h'
    include 'asterfort/dinona.h'
    include 'asterfort/dinonc.h'
    include 'asterfort/dinosi.h'
    include 'asterfort/disief.h'
    include 'asterfort/infdis.h'
    include 'asterfort/infted.h'
    include 'asterfort/jevech.h'
    include 'asterfort/matrot.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/rcvarc.h'
    include 'asterfort/tecach.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/ut2mgl.h'
    include 'asterfort/ut2mlg.h'
    include 'asterfort/ut2vgl.h'
    include 'asterfort/utpsgl.h'
    include 'asterfort/utpslg.h'
    include 'asterfort/utpvgl.h'
    include 'asterfort/vdiff.h'
    include 'blas/dcopy.h'
    character(len=16) :: option, nomte
    character(len=*) :: optioz, nomtez
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
! person_in_charge: jean-luc.flejou at edf.fr
!     ELEMENTS CONCERNES :  MECA_DIS_TR_L : SUR UNE MAILLE A 2 NOEUDS
!                           MECA_DIS_T_L  : SUR UNE MAILLE A 2 NOEUDS
!                           MECA_DIS_TR_N : SUR UNE MAILLE A 1 NOEUD
!                           MECA_DIS_T_N  : SUR UNE MAILLE A 1 NOEUD
!    ON CALCULE LES OPTIONS FULL_MECA
!                           RAPH_MECA
!                           RIGI_MECA_TANG
!     ELEMENTS CONCERNES :  MECA_2D_DIS_TR_L : SUR UNE MAILLE A 2 NOEUDS
!                           MECA_2D_DIS_T_L  : SUR UNE MAILLE A 2 NOEUDS
!                           MECA_2D_DIS_TR_N : SUR UNE MAILLE A 1 NOEUD
!                           MECA_2D_DIS_T_N  : SUR UNE MAILLE A 1 NOEUD
!    ON CALCULE LES OPTIONS FULL_MECA
!                           RAPH_MECA
!                           RIGI_MECA_TANG
!     POUR LE COMPORTEMENT  ELAS
! ----------------------------------------------------------------------
! IN  : OPTIOZ : NOM DE L'OPTION A CALCULER
!       NOMTEZ : NOM DU TYPE_ELEMENT
! ----------------------------------------------------------------------
!
!
! ----------------------------------------------------------------------
    real(kind=8) :: ang(3), pgl(3, 3), klv(78), klv2(78), xd(3)
    real(kind=8) :: ugm(12), ugp(12), dug(12)
    real(kind=8) :: ulm(12), ulp(12), dul(12), dvl(12), dpe(12), dve(12)
    real(kind=8) :: varmo(8), varpl(8), varipc(7)
    real(kind=8) :: dtemps, temper, r8bid
    real(kind=8) :: duly, force(3), plouf, varip
    real(kind=8) :: zero, moins1
!
    integer :: nbt, nno, nc, neq, igeom, icontm, ideplm, ideplp, icompo
    integer :: i, jdc, irep, imat, ifono, icontp, ilogic, iiter, imate
    integer :: irmetg, iterat, ivarip, ndim, jcret, itype, lorien, ivarim
    integer :: iadzi, iazk24, ibid, infodi, kpg, spt
    integer :: ii, jtp, jtm, idepen, iret, iviten, ivitp, jinst
!
    character(len=4) :: fami
    character(len=8) :: k8bid, famil, poum
    character(len=24) :: messak(5)
    parameter     (zero = 0.0d0, moins1=-1.0d0)
! ----------------------------------------------------------------------
!
!         COMPORTEMENT NON-LINEAIRE POUR LES DISCRETS
!
! ----------------------------------------------------------------------
    integer :: nbpar
    real(kind=8) :: valpa
    character(len=8) :: nompa
!
!     LOI VISQUEUSE SUR 6 COMPOSANTES
!     NBRE2  : 2 PARAMETRES PAR COMPOSANTE
!     NBVIN2 : 2 VARIABLES INTERNES PAR COMPOSANTES
    integer :: nbre2, nbvin2
    parameter   (nbre2  = 2*6 , nbvin2 = 2*6)
!     VALRE2 : VALEUR PARAMETRES DE LA LOI
!     NOMRE2 : NOM DES PARAMETRES DE LA LOI
    real(kind=8) :: valre2(nbre2)
    integer :: codre2(nbre2)
    character(len=8) :: nomre2(nbre2)
!
!     LOI CINEMATIQUE SUR 6 COMPOSANTES
!     NBRE3  : 4 PARAMETRES PAR COMPOSANTE
!     NBVIN3 : 3 VARIABLES INTERNES PAR COMPOSANTES
    integer :: nbre3, nbvin3
    parameter   (nbre3  = 4*6 , nbvin3 = 3*6)
!     VALRE2 : VALEUR PARAMETRES DE LA LOI
!     NOMRE2 : NOM DES PARAMETRES DE LA LOI
    real(kind=8) :: valre3(nbre3)
    integer :: codre3(nbre3)
    character(len=8) :: nomre3(nbre3)
!
!     LOI BI-LINEAIRE SUR 6 COMPOSANTES
!     NBRE4  : 3 PARAMETRES PAR COMPOSANTE
!     NBVIN4 : 1 VARIABLES INTERNES PAR COMPOSANTES
    integer :: nbre4, nbvin4
    parameter   (nbre4  = 3*6 , nbvin4 = 1*6)
!     VALRE4 : VALEUR PARAMETRES DE LA LOI
!     NOMRE4 : NOM DES PARAMETRES DE LA LOI
    real(kind=8) :: valre4(nbre4)
    integer :: codre4(nbre4)
    character(len=8) :: nomre4(nbre4)
!
!     LES DISCRETS ONT 6 DDL AU MAXIMUM
!     RAIDE : EXISTE POUR TOUS LES DISCRETS, TANGENTE AU COMPORTEMENT
!     OKDIRE : VRAI SI LE COMPORTEMENT AFFECTE CETTE DIRECTION
!     COEFLO(6,NBPARA) : PARAMETRES NECESSAIRES A LA LOI
!     NBPARA : NOMBRE MAXIMAL DE PARAMETRES TOUTES LOIS CONFONDUES
!        LOI 2 VISQUEUSE         NBRE2 = 2 PARAMETRES PAR COMPOSANTE
!        LOI 3 CINEMATIQUE       NBRE3 = 4 PARAMETRES PAR COMPOSANTE
!        LOI 4 BILINEAIRE        NBRE4 = 3 PARAMETRES PAR COMPOSANTE
    integer :: nbpara
    parameter   (nbpara=nbre3)
    real(kind=8) :: raide(6), coeflo(6, nbpara)
    logical :: okdire(6)
!
!     DOIT ETRE DIMENSIONNE EN COHERENCE AVEC LE CATALOGUE C_COMP_INCR
!     NOMBRE MAXIMAL DE VARIABLES INTERNES SUR LES DISCRETS
!              TOUTES LOIS CONFONDUES
!        LOI 2 VISQUEUX       NBVIN2= 2  : F+  Energie
!        LOI 3 CINEMATIQUE    NBVIN3= 3  : Uan  alpha  Energie
!        LOI 4 BILINIAIRE     NBVIN4= 1  : indicateur
    integer :: nbvint, numloi
    parameter   (nbvint = nbvin3)
    real(kind=8) :: vardnl(nbvint)
!     LOI 2 : VISQUEUSE         NBRE2  : 2 PARAMETRES PAR COMPOSANTE
    data nomre2 /'PUIS_DX','COEF_DX',&
     &             'PUIS_DY','COEF_DY',&
     &             'PUIS_DZ','COEF_DZ',&
     &             'PUIS_RX','COEF_RX',&
     &             'PUIS_RY','COEF_RY',&
     &             'PUIS_RZ','COEF_RZ'/
!     LOI 3 : CINEMATIQUE       NBRE3  : 4 PARAMETRES PAR COMPOSANTE
    data nomre3 /'LIMU_DX','PUIS_DX','KCIN_DX','LIMY_DX',&
     &             'LIMU_DY','PUIS_DY','KCIN_DY','LIMY_DY',&
     &             'LIMU_DZ','PUIS_DZ','KCIN_DZ','LIMY_DZ',&
     &             'LIMU_RX','PUIS_RX','KCIN_RX','LIMY_RX',&
     &             'LIMU_RY','PUIS_RY','KCIN_RY','LIMY_RY',&
     &             'LIMU_RZ','PUIS_RZ','KCIN_RZ','LIMY_RZ'/
!     LOI 4 : BILINEAIRE        NBRE4  : 3 PARAMETRES PAR COMPOSANTE
    data nomre4 /'KDEB_DX','KFIN_DX','FPRE_DX',&
     &             'KDEB_DY','KFIN_DY','FPRE_DY',&
     &             'KDEB_DZ','KFIN_DZ','FPRE_DZ',&
     &             'KDEB_RX','KFIN_RX','FPRE_RX',&
     &             'KDEB_RY','KFIN_RY','FPRE_RY',&
     &             'KDEB_RZ','KFIN_RZ','FPRE_RZ'/
!
! *********** FIN DES DECLARATIONS DES VARIABLES LOCALES ***************
!
!
! ********************* DEBUT DE LA SUBROUTINE *************************
!
    fami = 'RIGI'
    option = optioz
    nomte = nomtez
    famil='FPG1'
    kpg=1
    spt=1
    poum='+'
!
!     ON VERIFIE QUE LES CARACTERISTIQUES ONT ETE AFFECTEES
!     LE CODE DU DISCRET
    call infdis('CODE', ibid, r8bid, nomte)
!     LE CODE STOKE DANS LA CARTE
    call infdis('TYDI', infodi, r8bid, k8bid)
    if (infodi .ne. ibid) then
        call u2mesk('F+', 'DISCRETS_25', 1, nomte)
        call infdis('DUMP', ibid, r8bid, 'F+')
    endif
!     DISCRET DE TYPE RAIDEUR
    call infdis('DISK', infodi, r8bid, k8bid)
    if (infodi .eq. 0) then
        call u2mesk('A+', 'DISCRETS_27', 1, nomte)
        call infdis('DUMP', ibid, r8bid, 'A+')
    endif
!
!     LES DISCRETS SONT OBLIGATOIREMENT SYMETRIQUES
    call infdis('SYMK', infodi, r8bid, k8bid)
    if (infodi .ne. 1) then
        call u2mess('F', 'DISCRETS_40')
    endif
!
! --- INFORMATIONS SUR LES DISCRETS :
!        NBT   = NOMBRE DE COEFFICIENTS DANS K
!        NNO   = NOMBRE DE NOEUDS
!        NC    = NOMBRE DE COMPOSANTE PAR NOEUD
!        NDIM  = DIMENSION DE L'ELEMENT
!        ITYPE = TYPE DE L'ELEMENT
    call infted(nomte, infodi, nbt, nno, nc,&
                ndim, itype)
    neq = nno*nc
!
! --- RECUPERATION DES ADRESSES JEVEUX
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCONTMR', 'L', icontm)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PDEPLPR', 'L', ideplp)
    call jevech('PCOMPOR', 'L', icompo)
!--- RECUPERATION DES INFOS CONCERNANT LES COMPORTEMENTS
!                       12345678901234   12345678901234
!     ZK16(ICOMPO)      ELAS             DIS_GRICRA
!                       DIS_VISC         DIS_ECRO_CINE
!                       DIS_BILI_ELAS    ASSE_CORN
!                       ARME             DIS_CHOC
!                       DIS_GOUJ2E
!     ZK16(ICOMPO+1)    NBVAR = READ (ZK16(ICOMPO+1),'(I16)')
!     ZK16(ICOMPO+2)    PETIT
!     ZK16(ICOMPO+3)    COMP_ELAS   COMP_INCR
    if (zk16(icompo+2) .ne. 'PETIT') call u2mess('A', 'DISCRETS_18')
!
!         OPTION           MATRICE     FORCES
!      12345678901234      TANGENTE    NODALES
!      FULL_MECA             OUI        OUI
!      RAPH_MECA                        OUI
!      RIGI_MECA_TANG        OUI
!      RIGI_MECA_ELAS        OUI
!      FULL_MECA_ELAS        OUI
!
!     On peut avoir COMP_ELAS et seulement comportement ELAS
    if ((zk16(icompo+3).eq.'COMP_ELAS') .and. (zk16(icompo).ne.'ELAS')) then
        messak(1) = nomte
        messak(2) = option
        messak(3) = zk16(icompo+3)
        messak(4) = zk16(icompo)
        call tecael(iadzi, iazk24)
        messak(5) = zk24(iazk24-1+3)
        call u2mesk('F', 'DISCRETS_8', 5, messak)
    endif
!
!     Dans les cas *_ELAS, les comportements qui ont une matrice de
!     décharge sont : ELAS DIS_GRICRA
    if ((option(10:14).eq.'_ELAS') .and. (zk16(icompo).ne.'ELAS') .and.&
        (zk16(icompo).ne.'DIS_GRICRA')) then
        messak(1) = nomte
        messak(2) = option
        messak(3) = zk16(icompo+3)
        messak(4) = zk16(icompo)
        call tecael(iadzi, iazk24)
        messak(5) = zk24(iazk24-1+3)
        call u2mesk('F', 'DISCRETS_10', 5, messak)
    endif
!
! ======================================================================
!   ORIENTATION DE L'ELEMENT ET DEPLACEMENTS DANS LES REPERES G ET L
! ======================================================================
! --- RECUPERATION DES ORIENTATIONS (ANGLES NAUTIQUES -> VECTEUR ANG)
    call tecach('ONN', 'PCAORIE', 'L', 1, lorien,&
                iret)
    if (iret .ne. 0) then
        messak(1) = nomte
        messak(2) = option
        messak(3) = zk16(icompo+3)
        messak(4) = zk16(icompo)
        call tecael(iadzi, iazk24)
        messak(5) = zk24(iazk24-1+3)
        call u2mesk('F', 'DISCRETS_6', 5, messak)
    endif
    call dcopy(3, zr(lorien), 1, ang, 1)
!
! --- DEPLACEMENTS DANS LE REPERE GLOBAL
!        UGM = DEPLACEMENT PRECEDENT
!        DUG = INCREMENT DE DEPLACEMENT
!        UGP = DEPLACEMENT COURANT
!        XD  = VECTEUR JOIGNANT LES DEUX NOEUDS AU REPOS
    do 10 i = 1, neq
        ugm(i) = zr(ideplm+i-1)
        dug(i) = zr(ideplp+i-1)
        ugp(i) = ugm(i) + dug(i)
10  end do
!
    if (nno .eq. 2) then
        call vdiff(ndim, zr(igeom+ndim), zr(igeom), xd)
    endif
!
! --- MATRICE MGL DE PASSAGE REPERE GLOBAL -> REPERE LOCAL
    call matrot(ang, pgl)
!
! --- DEPLACEMENTS DANS LE REPERE LOCAL
!        ULM = DEPLACEMENT PRECEDENT    = PLG * UGM
!        DUL = INCREMENT DE DEPLACEMENT = PLG * DUG
!        ULP = DEPLACEMENT COURANT      = PLG * UGP
    if (ndim .eq. 3) then
        call utpvgl(nno, nc, pgl, ugm, ulm)
        call utpvgl(nno, nc, pgl, dug, dul)
        call utpvgl(nno, nc, pgl, ugp, ulp)
    else if (ndim.eq.2) then
        call ut2vgl(nno, nc, pgl, ugm, ulm)
        call ut2vgl(nno, nc, pgl, dug, dul)
        call ut2vgl(nno, nc, pgl, ugp, ulp)
    endif
!
! ======================================================================
!                      COMPORTEMENT ELASTIQUE
! ======================================================================
    if (zk16(icompo) .eq. 'ELAS') then
! ------ PARAMETRES EN ENTREE
        call jevech('PCADISK', 'L', jdc)
        call infdis('REPK', irep, r8bid, k8bid)
!        ABSOLU VERS LOCAL ? ---
!        IREP = 1 = MATRICE EN REPERE GLOBAL ==> PASSER EN LOCAL ---
        if (irep .eq. 1) then
            if (ndim .eq. 3) then
                call utpsgl(nno, nc, pgl, zr(jdc), klv)
            else if (ndim.eq.2) then
                call ut2mgl(nno, nc, pgl, zr(jdc), klv)
            endif
        else
            call dcopy(nbt, zr(jdc), 1, klv, 1)
        endif
!
! --- CALCUL DE LA MATRICE TANGENTE
        if (option(1: 9) .eq. 'FULL_MECA' .or. option(1:10) .eq. 'RIGI_MECA_') then
            call jevech('PMATUUR', 'E', imat)
            if (ndim .eq. 3) then
                call utpslg(nno, nc, pgl, klv, zr(imat))
            else if (ndim.eq.2) then
                call ut2mlg(nno, nc, pgl, klv, zr(imat))
            endif
        endif
!
! --- CALCUL DES EFFORTS GENERALISES ET DES FORCES NODALES
        if (option(1:9) .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
            call jevech('PVECTUR', 'E', ifono)
            call jevech('PCONTPR', 'E', icontp)
            ilogic = 0
            plouf = 0.d0
            call r8inir(3, zero, force, 1)
            call disief(nbt, neq, nno, nc, pgl,&
                        klv, dul, zr(icontm), ilogic, plouf,&
                        zr(icontp), zr(ifono), force, ndim)
        endif
        goto 800
    endif
! ======================================================================
!                  FIN DU COMPORTEMENT ELASTIQUE
! ======================================================================
!
! ======================================================================
!            DEBUT COMPORTEMENT DIS_VISC : DISCRET_NON_LINE
! ======================================================================
    if (zk16(icompo) .eq. 'DIS_VISC') then
! ------ RECUPERATION DU MATERIAU
        call jevech('PMATERC', 'L', imate)
!
! ------ VARIABLES INTERNES A T-
        call jevech('PVARIMR', 'L', ivarim)
! ------ RECUPERATION DES CARACTERISTIQUES ELASTIQUE
        call jevech('PCADISK', 'L', jdc)
        call infdis('REPK', irep, r8bid, k8bid)
!
        numloi = 2
        nbpar = 0
        nompa = ' '
        valpa = 0.d0
! --- -- RECUPERE TOUS LES PARAMETRES
        call r8inir(nbre2, zero, valre2, 1)
        call rcvalb(famil, kpg, spt, poum, zi(imate),&
                    ' ', 'DIS_VISC', nbpar, nompa, valpa,&
                    nbre2, nomre2, valre2, codre2, 0)
!
! --- -- MATRICE EN REPERE GLOBAL ==> IREP = 1
        if (irep .eq. 1) then
            messak(1) = nomte
            messak(2) = option
            messak(3) = zk16(icompo+3)
            messak(4) = zk16(icompo)
            call tecael(iadzi, iazk24)
            messak(5) = zk24(iazk24-1+3)
            call u2mesk('F', 'DISCRETS_5', 5, messak)
        endif
!
! --- -- LES CARACTERISTIQUES SONT TOUJOURS DANS LE REPERE LOCAL
!        ON FAIT SEULEMENT UNE COPIE
        call dcopy(nbt, zr(jdc), 1, klv, 1)
!
! --- -- SI UN DDL N'EST PAS AFFECTE D'UN COMPORTEMENT NON-LINEAIRE
!        IL EST DONC ELASTIQUE DANS CETTE DIRECTION. ==> DINONC
        call r8inir(6, zero, raide, 1)
! --- -- EXAMEN DES CODRE2, VALRE2. ON AFFECTE RAIDE, LES PARAMETRES
        call dinonc(nomte, codre2, valre2, klv, raide,&
                    nbpara, coeflo, 2, okdire)
!
! --- -- LOI DE COMPORTEMENT NON-LINEAIRE
        call r8inir(nbvint, zero, vardnl, 1)
!        RECUPERATION DU TEMPS + et -. CALCUL DE DT
        call jevech('PINSTPR', 'L', jtp)
        call jevech('PINSTMR', 'L', jtm)
        dtemps = zr(jtp) - zr(jtm)
        call dinon2(neq, ulm, dul, ulp, nno,&
                    nc, zr(ivarim), raide, nbpara, coeflo,&
                    okdire, vardnl, dtemps)
!
! --- -- ACTUALISATION DE LA MATRICE QUASI-TANGENTE
        call dinona(nomte, raide, klv)
!
!        ACTUALISATION DE LA MATRICE QUASI-TANGENTE
        if (option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
            call jevech('PMATUUR', 'E', imat)
            if (ndim .eq. 3) then
                call utpslg(nno, nc, pgl, klv, zr(imat))
            else if (ndim.eq.2) then
                call ut2mlg(nno, nc, pgl, klv, zr(imat))
            endif
        endif
!
! ------ CALCUL DES EFFORTS GENERALISES, DES FORCES NODALES
!        ET MISE A JOUR DES VARIABLES INTERNES
        if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
            call jevech('PVECTUR', 'E', ifono)
            call jevech('PCONTPR', 'E', icontp)
            call jevech('PVARIPR', 'E', ivarip)
!
            call dinosi(nbt, neq, nno, nc, pgl,&
                        klv, dul, zr(icontm), zr( icontp), zr(ifono),&
                        numloi, vardnl)
            do 122 ii = 1, nbvin2
                zr(ivarip+ii-1) = vardnl(ii)
122          continue
        endif
        goto 800
    endif
! ======================================================================
!            FIN COMPORTEMENT VISQUEUX DISCRET_NON_LINE
! ======================================================================
!
! ======================================================================
!            DEBUT COMPORTEMENT DIS_ECRO_CINE : DISCRET_NON_LINE
! ======================================================================
    if (zk16(icompo) .eq. 'DIS_ECRO_CINE') then
! ------ RECUPERATION DU MATERIAU
        call jevech('PMATERC', 'L', imate)
!
! ------ VARIABLES INTERNES A T-
        call jevech('PVARIMR', 'L', ivarim)
! ------ RECUPERATION DES CARACTERISTIQUES
        call jevech('PCADISK', 'L', jdc)
        call infdis('REPK', irep, r8bid, k8bid)
!
        numloi = 3
        nbpar = 0
        nompa = ' '
        valpa = 0.d0
! --- -- RECUPERE TOUS LES PARAMETRES
        call r8inir(nbre3, zero, valre3, 1)
        call rcvalb(famil, kpg, spt, poum, zi(imate),&
                    ' ', 'DIS_ECRO_CINE', nbpar, nompa, valpa,&
                    nbre3, nomre3, valre3, codre3, 0)
!
! --- -- MATRICE EN REPERE GLOBAL ==> IREP = 1
        if (irep .eq. 1) then
            messak(1) = nomte
            messak(2) = option
            messak(3) = zk16(icompo+3)
            messak(4) = zk16(icompo)
            call tecael(iadzi, iazk24)
            messak(5) = zk24(iazk24-1+3)
            call u2mesk('F', 'DISCRETS_5', 5, messak)
        endif
!
! --- -- LES CARACTERISTIQUES SONT TOUJOURS DANS LE REPERE LOCAL
!        ON FAIT SEULEMENT UNE COPIE
        call dcopy(nbt, zr(jdc), 1, klv, 1)
!
! --- -- SI UN DDL N'EST PAS AFFECTE D'UN COMPORTEMENT NON-LINEAIRE
!        IL EST DONC ELASTIQUE DANS CETTE DIRECTION. ==> DINONC
        call r8inir(6, zero, raide, 1)
        call r8inir(6*nbpara, moins1, coeflo, 1)
! --- -- EXAMEN DES CODRE3, VALRE3. ON AFFECTE RAIDE, LES PARAMETRES
        call dinonc(nomte, codre3, valre3, klv, raide,&
                    nbpara, coeflo, 4, okdire)
! --- -- LOI DE COMPORTEMENT NON-LINEAIRE
        call r8inir(nbvint, zero, vardnl, 1)
        call dinon3(neq, ulm, dul, ulp, nno,&
                    nc, zr(ivarim), raide, nbpara, coeflo,&
                    okdire, vardnl)
!
! --- -- ACTUALISATION DE LA MATRICE QUASI-TANGENTE
        call dinona(nomte, raide, klv)
!
!        ACTUALISATION DE LA MATRICE QUASI-TANGENTE
        if (option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
            call jevech('PMATUUR', 'E', imat)
            if (ndim .eq. 3) then
                call utpslg(nno, nc, pgl, klv, zr(imat))
            else if (ndim.eq.2) then
                call ut2mlg(nno, nc, pgl, klv, zr(imat))
            endif
        endif
!
! ------ CALCUL DES EFFORTS GENERALISES, DES FORCES NODALES
!        ET MISE A JOUR DES VARIABLES INTERNES
        if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
            call jevech('PVECTUR', 'E', ifono)
            call jevech('PCONTPR', 'E', icontp)
            call jevech('PVARIPR', 'E', ivarip)
!
            call dinosi(nbt, neq, nno, nc, pgl,&
                        klv, dul, zr(icontm), zr( icontp), zr(ifono),&
                        numloi, vardnl)
            do 123 ii = 1, nbvin3
                zr(ivarip+ii-1) = vardnl(ii)
123          continue
        endif
        goto 800
    endif
! ======================================================================
!              FIN COMPORTEMENT CINEMATIQUE DISCRET_NON_LINE
! ======================================================================
!
! ======================================================================
!            DEBUT COMPORTEMENT DIS_BILI_ELAS : DISCRET_NON_LINE
! ======================================================================
    if (zk16(icompo) .eq. 'DIS_BILI_ELAS') then
! ------ RECUPERATION DU MATERIAU
        call jevech('PMATERC', 'L', imate)
!
! ------ VARIABLES INTERNES A T-
        call jevech('PVARIMR', 'L', ivarim)
! ------ RECUPERATION DES CARACTERISTIQUES
        call jevech('PCADISK', 'L', jdc)
        call infdis('REPK', irep, r8bid, k8bid)
!
! --- -- MATRICE EN REPERE GLOBAL ==> IREP = 1
        if (irep .eq. 1) then
            messak(1) = nomte
            messak(2) = option
            messak(3) = zk16(icompo+3)
            messak(4) = zk16(icompo)
            call tecael(iadzi, iazk24)
            messak(5) = zk24(iazk24-1+3)
            call u2mesk('F', 'DISCRETS_5', 5, messak)
        endif
! --- -- LES CARACTERISTIQUES SONT TOUJOURS DANS LE REPERE LOCAL
!        ON FAIT SEULEMENT UNE COPIE
        call dcopy(nbt, zr(jdc), 1, klv, 1)
!
        numloi = 4
! --- -- RECUPERE TOUS LES PARAMETRES
        call r8inir(nbre4, zero, valre4, 1)
! --- -- TEMPERATURE : SI 2 NOEUDS ==> MOYENNE
        call rcvarc(' ', 'TEMP', '+', 'RIGI', 1,&
                    1, valpa, iret)
        if (nno .eq. 2) then
            call rcvarc(' ', 'TEMP', '+', 'RIGI', 2,&
                        1, temper, iret)
            valpa = (valpa+temper)*0.5d0
        endif
        nbpar = 1
        nompa = 'TEMP'
        call rcvalb(famil, kpg, spt, poum, zi(imate),&
                    ' ', 'DIS_BILI_ELAS', nbpar, nompa, valpa,&
                    nbre4, nomre4, valre4, codre4, 0)
!
! --- -- SI UN DDL N'EST PAS AFFECTE D'UN COMPORTEMENT NON-LINEAIRE
!        IL EST DONC ELASTIQUE DANS CETTE DIRECTION. ==> DINONC
        call r8inir(6, zero, raide, 1)
! --- -- EXAMEN DES CODRE4 VALRE4. ON AFFECTE RAIDE, LES PARAMETRES
        call dinonc(nomte, codre4, valre4, klv, raide,&
                    nbpara, coeflo, 3, okdire)
!
! --- -- LOI DE COMPORTEMENT NON-LINEAIRE
        call r8inir(nbvint, zero, vardnl, 1)
        call dinon4(neq, ulm, dul, ulp, nno,&
                    nc, zr(ivarim), raide, nbpara, coeflo,&
                    okdire, vardnl)
!
! --- -- ACTUALISATION DE LA MATRICE QUASI-TANGENTE
        call dinona(nomte, raide, klv)
!
!        ACTUALISATION DE LA MATRICE QUASI-TANGENTE
        if (option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
            call jevech('PMATUUR', 'E', imat)
            if (ndim .eq. 3) then
                call utpslg(nno, nc, pgl, klv, zr(imat))
            else if (ndim.eq.2) then
                call ut2mlg(nno, nc, pgl, klv, zr(imat))
            endif
        endif
!
! ------ CALCUL DES EFFORTS GENERALISES, DES FORCES NODALES
!        ET MISE A JOUR DES VARIABLES INTERNES
        if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
            call jevech('PVECTUR', 'E', ifono)
            call jevech('PCONTPR', 'E', icontp)
            call jevech('PVARIPR', 'E', ivarip)
!
            call dinosi(nbt, neq, nno, nc, pgl,&
                        klv, dul, zr(icontm), zr( icontp), zr(ifono),&
                        numloi, vardnl)
            do 124 ii = 1, nbvin4
                zr(ivarip+ii-1) = vardnl(ii)
124          continue
        endif
        goto 800
    endif
! ======================================================================
!              FIN COMPORTEMENT DIS_BILI_ELAS DISCRET_NON_LINE
! ======================================================================
!
! ======================================================================
!                      COMPORTEMENT CORNIERE
! ======================================================================
    if (zk16(icompo) .eq. 'ASSE_CORN') then
! ---    PARAMETRES EN ENTREE
        call jevech('PITERAT', 'L', iiter)
        call jevech('PMATERC', 'L', imate)
        call jevech('PVARIMR', 'L', ivarim)
! ---    RELATION DE COMPORTEMENT DE LA CORNIERE
        irmetg = 0
        if (option .eq. 'RIGI_MECA_TANG') irmetg = 1
        iterat = nint(zr(iiter))
        call dicorn(irmetg, nbt, neq, iterat, zi(imate),&
                    ulm, dul, ulp, zr(icontm), zr(ivarim),&
                    klv, klv2, varipc)
!
! ---    ACTUALISATION DE LA MATRICE TANGENTE
        if (option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
            call jevech('PMATUUR', 'E', imat)
            call utpslg(nno, nc, pgl, klv2, zr(imat))
        endif
!
! ---    CALCUL DES EFFORTS GENERALISES, DES FORCES NODALES
!        ET DES VARIABLES INTERNES
        if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
            call jevech('PVECTUR', 'E', ifono)
            call jevech('PCONTPR', 'E', icontp)
            call jevech('PVARIPR', 'E', ivarip)
            ilogic = 0
            plouf = 0.d0
            call r8inir(3, zero, force, 1)
            call disief(nbt, neq, nno, nc, pgl,&
                        klv, dul, zr(icontm), ilogic, plouf,&
                        zr(icontp), zr(ifono), force, ndim)
            do 25 i = 1, 7
                zr(ivarip+i-1) = varipc(i)
                zr(ivarip+i+6) = varipc(i)
25          continue
        endif
        goto 800
    endif
! ======================================================================
!                 FIN DU COMPORTEMENT CORNIERE
! ======================================================================
!
! ======================================================================
!                    COMPORTEMENT ARMEMENT
! ======================================================================
    if (zk16(icompo) .eq. 'ARME') then
! ---    PARAMETRES EN ENTREE
        call jevech('PCADISK', 'L', jdc)
        call infdis('REPK', irep, r8bid, k8bid)
        call dcopy(nbt, zr(jdc), 1, klv, 1)
        if (irep .eq. 1) then
            call utpsgl(nno, nc, pgl, zr(jdc), klv)
        endif
        call jevech('PMATERC', 'L', imate)
        call jevech('PVARIMR', 'L', ivarim)
! ---    RELATION DE COMPORTEMENT DE L'ARMEMENT
        call r8inir(3, zero, force, 1)
        call diarme(nbt, neq, zi(imate), ulm, dul,&
                    ulp, zr(icontm), zr( ivarim), klv, varip,&
                    force(1), duly)
! ---    ACTUALISATION DE LA MATRICE TANGENTE
        if (option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
            call jevech('PMATUUR', 'E', imat)
            call utpslg(nno, nc, pgl, klv, zr(imat))
        endif
! ---    CALCUL DES EFFORTS GENERALISES, DES FORCES NODALES
!        ET DES VARIABLES INTERNES
        if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
            call jevech('PVECTUR', 'E', ifono)
            call jevech('PCONTPR', 'E', icontp)
            call jevech('PVARIPR', 'E', ivarip)
            ilogic = 1
            call disief(nbt, neq, nno, nc, pgl,&
                        klv, dul, zr(icontm), ilogic, duly,&
                        zr(icontp), zr(ifono), force, ndim)
            zr(ivarip) = varip
            zr(ivarip+1) = varip
        endif
        goto 800
    endif
! ======================================================================
!                 FIN DU COMPORTEMENT ARMEMENT
! ======================================================================
!
! ======================================================================
!  COMPORTEMENT DIS_GRICRA : APPLICATION : LIAISON GRILLE-CRAYON COMBU
! ======================================================================
    if (zk16(icompo) .eq. 'DIS_GRICRA') then
        if (nomte .ne. 'MECA_DIS_TR_L') then
            messak(1) = nomte
            messak(2) = option
            messak(3) = zk16(icompo+3)
            messak(4) = zk16(icompo)
            call tecael(iadzi, iazk24)
            messak(5) = zk24(iazk24-1+3)
            call u2mesk('F', 'DISCRETS_11', 5, messak)
        endif
! ---    PARAMETRES EN ENTREE
        call jevech('PMATERC', 'L', imate)
        call jevech('PVARIMR', 'L', ivarim)
        call jevech('PINSTPR', 'L', jtp)
!
        if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
            call jevech('PVECTUR', 'E', ifono)
            call jevech('PCONTPR', 'E', icontp)
            call jevech('PVARIPR', 'E', ivarip)
        else
            ifono=1
            icontp=1
            ivarip=1
        endif
        call dicrgr(fami, option, neq, nc, zi(imate),&
                    ulm, dul, zr(icontm), zr(ivarim), pgl,&
                    klv, zr(ivarip), zr(ifono), zr(icontp))
!
        if (option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
            call jevech('PMATUUR', 'E', imat)
            call utpslg(nno, nc, pgl, klv, zr(imat))
        endif
        goto 800
    endif
! ======================================================================
!                 FIN DU COMPORTEMENT DIS_GRICRA
! ======================================================================
!
! ======================================================================
!                    COMPORTEMENT CHOC
! ======================================================================
    if (zk16(icompo) .eq. 'DIS_CHOC') then
! ---    PARAMETRES EN ENTREE
        call jevech('PCADISK', 'L', jdc)
        call infdis('REPK', irep, r8bid, k8bid)
!        ABSOLU VERS LOCAL ? ---
!        IREP = 1 = MATRICE EN REPERE GLOBAL ==> PASSER EN LOCAL ---
        if (irep .eq. 1) then
            if (ndim .eq. 3) then
                call utpsgl(nno, nc, pgl, zr(jdc), klv)
            else if (ndim.eq.2) then
                call ut2mgl(nno, nc, pgl, zr(jdc), klv)
            endif
        else
            call dcopy(nbt, zr(jdc), 1, klv, 1)
        endif
        call jevech('PMATERC', 'L', imate)
        call jevech('PVARIMR', 'L', ivarim)
        do 30 i = 1, 8
            varmo(i) = zr(ivarim+i-1)
30      continue
        call jevech('PINSTPR', 'L', jinst)
        call tecach('ONN', 'PVITPLU', 'L', 1, ivitp,&
                    iret)
        if (iret .eq. 0) then
            if (ndim .eq. 3) then
                call utpvgl(nno, nc, pgl, zr(ivitp), dvl)
            else if (ndim.eq.2) then
                call ut2vgl(nno, nc, pgl, zr(ivitp), dvl)
            endif
        else
            do 40 i = 1, 12
                dvl(i) = 0.d0
40          continue
        endif
        call tecach('ONN', 'PDEPENT', 'L', 1, idepen,&
                    iret)
        if (iret .eq. 0) then
            if (ndim .eq. 3) then
                call utpvgl(nno, nc, pgl, zr(idepen), dpe)
            else if (ndim.eq.2) then
                call ut2vgl(nno, nc, pgl, zr(idepen), dpe)
            endif
        else
            do 50 i = 1, 12
                dpe(i) = 0.d0
50          continue
        endif
        call tecach('ONN', 'PVITENT', 'L', 1, iviten,&
                    iret)
        if (iret .eq. 0) then
            if (ndim .eq. 3) then
                call utpvgl(nno, nc, pgl, zr(iviten), dve)
            else if (ndim.eq.2) then
                call ut2vgl(nno, nc, pgl, zr(iviten), dve)
            endif
        else
            do 60 i = 1, 12
                dve(i) = 0.d0
60          continue
        endif
! ---    RELATION DE COMPORTEMENT DE CHOC
        call dichoc(nbt, neq, nno, nc, zi(imate),&
                    dul, ulp, zr(igeom), pgl, klv,&
                    duly, dvl, dpe, dve, force,&
                    varmo, varpl, ndim)
! ---    ACTUALISATION DE LA MATRICE TANGENTE
        if (option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
            call jevech('PMATUUR', 'E', imat)
            if (ndim .eq. 3) then
                call utpslg(nno, nc, pgl, klv, zr(imat))
            else if (ndim.eq.2) then
                call ut2mlg(nno, nc, pgl, klv, zr(imat))
            endif
        endif
!
! ---    CALCUL DES EFFORTS GENERALISES, DES FORCES NODALES
!        ET DES VARIABLES INTERNES
        if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
            call jevech('PVECTUR', 'E', ifono)
            call jevech('PCONTPR', 'E', icontp)
            call jevech('PVARIPR', 'E', ivarip)
            ilogic = 2
            call disief(nbt, neq, nno, nc, pgl,&
                        klv, dul, zr(icontm), ilogic, duly,&
                        zr(icontp), zr(ifono), force, ndim)
            do 70 i = 1, 8
                zr(ivarip+i-1) = varpl(i)
                if (nno .eq. 2) zr(ivarip+i+7) = varpl(i)
70          continue
        endif
        goto 800
    endif
! ======================================================================
!                 FIN DU COMPORTEMENT CHOC
! ======================================================================
!
! ======================================================================
!  COMPORTEMENT DIS_GOUJON : APPLICATION : GOUJ2ECH
! ======================================================================
    if (zk16(icompo) (1:10) .eq. 'DIS_GOUJ2E') then
! ---    PARAMETRES EN ENTREE
        call jevech('PCADISK', 'L', jdc)
!        MATRICE DE RIGIDITE EN REPERE LOCAL
        call infdis('REPK', irep, r8bid, k8bid)
        if (irep .eq. 1) then
            if (ndim .eq. 3) then
                call utpsgl(nno, nc, pgl, zr(jdc), klv)
            else if (ndim.eq.2) then
                call ut2mgl(nno, nc, pgl, zr(jdc), klv)
            endif
        else
            call dcopy(nbt, zr(jdc), 1, klv, 1)
        endif
        if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
            call jevech('PMATERC', 'L', imate)
            call jevech('PVARIMR', 'L', ivarim)
            call jevech('PVECTUR', 'E', ifono)
            call jevech('PCONTPR', 'E', icontp)
            call jevech('PVARIPR', 'E', ivarip)
        else if (option.eq.'RIGI_MECA_TANG') then
            call jevech('PMATERC', 'L', imate)
            call jevech('PVARIMR', 'L', ivarim)
            ifono=1
            icontp=1
            ivarip=1
        endif
!        RELATION DE COMPORTEMENT : ELASTIQUE PARTOUT
!        SAUF SUIVANT Y LOCAL : ELASTOPLASTIQUE VMIS_ISOT_TRAC
        call digouj(option, zk16(icompo), nno, nbt, neq,&
                    nc, zi(imate), dul, zr(icontm), zr(ivarim),&
                    pgl, klv, klv2, zr(ivarip), zr(ifono),&
                    zr(icontp), nomte)
!
        if (option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
            call jevech('PMATUUR', 'E', imat)
            if (ndim .eq. 3) then
                call utpslg(nno, nc, pgl, klv, zr(imat))
            else if (ndim.eq.2) then
                call ut2mlg(nno, nc, pgl, klv, zr(imat))
            endif
        endif
        goto 800
    endif
! ======================================================================
!                 FIN DU COMPORTEMENT DIS_GOUJON
! ======================================================================
!
!     SI ON PASSE PAR ICI C'EST QU'AUCUN COMPORTEMENT N'EST VALIDE
    messak(1) = nomte
    messak(2) = option
    messak(3) = zk16(icompo+3)
    messak(4) = zk16(icompo)
    call tecael(iadzi, iazk24)
    messak(5) = zk24(iazk24-1+3)
    call u2mesk('F', 'DISCRETS_7', 5, messak)
!
!     TOUS LES COMPORTEMENTS PASSE PAR ICI
800  continue
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = 0
    endif
!
end subroutine
