subroutine ajellt(ligrez, nomaz, nbma, limaz, typelz,&
                  phenoz, modelz, nbno, linoz)
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
!
!
! ======================================================================
! ------------------------------------------------------------
!     BUT : AFFECTATION DE L'OBJET DE TYPE LIGRET
!           ET DE NOM LIGREZ
!           ON STOCKE LA LISTE DE MAILLES LIMA DANS LE VECTEUR
!           LIGRET//'.LIMA'
!           QUAND IL S'AGIT DE MAILLES TARDIVES, ON A LIMA(1) = 0,
!           ALORS ON STOCKE LES NOEUDS DE LA MAILLE TARDIVE DANS
!           LE VECTEUR LIGRET//'.LINO'
!           QUAND TYPEL N'EST PAS DEFINI, ON RECUPERE LE TYPE DES
!           MAILLES VIA LA MODELISATION ET LE PHENOMENE .
!
!
!  ARGUMENT       E/S    TYPE          ROLE
!
!  LIGREZ         IN      K19     NOM DU LIGRET A AFFECTER
!                 JXVAR
!  NOMAZ          IN      K8      NOM DU MAILLAGE SUR LEQUEL S'APPUIE
!                                 LE LIGRET
!  NBMA           IN      I       NOMBRE DE MAILLES A AFFECTER
!  LIMAZ          IN      K24     NOM DU VECTEUR JEVEUX CONTENANT
!                                 LA LISTE DES NUMEROS DE MAILLES
!  TYPELZ         IN      K16     TYPE DES MAILLES A AFFECTER
!  PHENOZ         IN      K16     PHENOMENE ASSOCIE AU MODELE
!  MODELZ         IN      K16     MODELISATION ASSOCIEE AU MODELE
!  NBNO           IN      I       NOMBRE DE NOEUDS DE LA MAILLE
!                                 TARDIVE A AFFECTER
!  LINOZ          IN      K24     NOM DU VECTEUR JEVEUX CONTENANT
!                                 LA LISTE DES NUMEROS DE NOEUDS
!-------------------------------------------------------------
!
! ====================== DEBUT DES DECLARATIONS ========================
    include 'jeveux.h'
!
    include 'asterfort/crelgt.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/juveca.h'
    include 'asterfort/wkvect.h'
!
! ----- ARGUMENTS
    character(len=*) :: ligrez, nomaz, typelz, phenoz, modelz, limaz, linoz
! ----- VARIABLES LOCALES -------------------------------
!-----------------------------------------------------------------------
    integer :: i, idapma, idapno, idlima, idlino, idlity, idmata
    integer :: idmode, idnbma, idnoma, idphen, idpoma, idpono, imodl
    integer :: iret, iret1, iret2, ityp, jdlima, jdlino, jdpm
    integer :: jdtm, lolima, lolimx, lolino, lolinx, lopomx, loponx
    integer :: matard, nbma, nbmadi, nbmail, nbmax, nbno, nbnodi
    integer :: nbnox, nlolim, nlolin, numail, nutypm
!-----------------------------------------------------------------------
    parameter (nbmail = 10000)
!
    character(len=8) :: noma, k8bid
    character(len=16) :: pheno, modeli, typel
    character(len=19) :: ligret
    character(len=24) :: lima, lino, typmai
! ====================== DEBUT DU CODE EXECUTABLE ======================
!
    call jemarq()
!
! --- INITIALISATIONS :
!     ---------------
    noma = nomaz
    pheno = phenoz
    modeli = modelz
    ligret = ligrez
    typel = typelz
    lima = limaz
    lino = linoz
    typmai = noma//'.TYPMAIL'
!
    matard = 0
!
! --- ON VERIFIE SI LE LIGREL EXISTE ,S'IL N'EXISTE PAS, ON LE CREE :
!     -------------------------------------------------------------
    call jeexin(ligret//'.LGRF', iret)
!
    if (iret .eq. 0) then
        call crelgt('V', ligret)
    endif
!
! --- VECTEUR DE LA LISTE DES MAILLES CUMULEES DU LIGRET :
!     --------------------------------------------------
    call jeveuo(ligret//'.LIMA', 'E', idlima)
!
! --- VECTEUR DES TYPES DES MAILLES CUMULEES DU LIGRET :
!     ------------------------------------------------
    call jeveuo(ligret//'.LITY', 'E', idlity)
!
! --- NOM DE LA MODELISATION :
!     ----------------------
    call jeveuo(ligret//'.MODE', 'E', idmode)
!
! --- NOM DU PHENOMENE :
!     ----------------
    call jeveuo(ligret//'.PHEN', 'E', idphen)
!
! --- TABLEAU DE POINTEURS DANS LA LISTE DES MAILLES :
!     ----------------------------------------------
    call jeveuo(ligret//'.POMA', 'E', idpoma)
!
! --- TABLEAU DE POINTEURS DANS LA LISTE DES NOEUDS :
!     ---------------------------------------------
    call jeveuo(ligret//'.PONO', 'E', idpono)
!
! --- NOM DU MAILLAGE :
!     ---------------
    call jeveuo(ligret//'.LGRF', 'E', idnoma)
!
! --- NOMBRE DE MAILLES TARDIVES :
!     --------------------------
    call jeveuo(ligret//'.MATA', 'E', idmata)
!
! --- VECTEUR DE LA LISTE DES NOEUDS CUMULES DU LIGRET :
!     ------------------------------------------------
    call jeveuo(ligret//'.LINO', 'E', idlino)
!
! --- NOMBRE D'AFFECTATIONS DE MAILLES :
!     --------------------------------
    call jeveuo(ligret//'.APMA', 'E', idapma)
!
! --- NOMBRE D'AFFECTATIONS DE NOEUDS :
!     -------------------------------
    call jeveuo(ligret//'.APNO', 'E', idapno)
!
! --- NOMBRE D'AFFECTATIONS DE MAILLES :
!     --------------------------------
    call jeveuo(ligret//'.NBMA', 'E', idnbma)
!
    zi(idnbma) = zi(idnbma) + nbma
!
! --- ON AFFECTE UNE FOIS POUR TOUTES LE NOM DU MAILLAGE :
!     --------------------------------------------------
    if (iret .eq. 0) then
        zk8(idnoma) = noma
    endif
!
! --- RECUPERATION DE LA LISTE DES MAILLES A AFFECTER :
!     ===============================================
    if (lima .ne. ' ') then
        call jeexin(lima, iret1)
        if (iret1 .eq. 0) then
            call wkvect(lima, 'V V I', 1, jdlima)
        else
            call jeveuo(lima, 'L', jdlima)
        endif
    endif
!
! --- RECUPERATION DE LA LISTE DES NOEUDS A AFFECTER :
!     ===============================================
    if (lino .ne. ' ') then
        call jeexin(lino, iret2)
        if (iret2 .eq. 0) then
            call wkvect(lino, 'V V I', 1, jdlino)
        else
            call jeveuo(lino, 'L', jdlino)
        endif
    endif
!
! --- VERIFICATION DE L'ADEQUATION DE L'AFFECTATION DES MAILLES
! --- A LA LISTE DES MAILLES CUMULEES :
!     ===============================
    if (zi(jdlima) .gt. 0 .and. nbma .ge. 1) then
!
! ---   NOMBRE DE MAILLES DEJA AFFECTEES :
!       --------------------------------
        call jelira(ligret//'.LIMA', 'LONUTI', lolima, k8bid)
!
! ---   LONGUEUR DU VECTEUR LIGRET.LIMA :
!       -------------------------------
        call jelira(ligret//'.LIMA', 'LONMAX', lolimx, k8bid)
!
! ---   NOMBRE DE MAILLES DISPONIBLES :
!       -----------------------------
        nbmadi = lolimx - lolima
!
! ---   REAJUSTEMENT EVENTUEL DES VECTEURS LIMA ET LITY :
!       -----------------------------------------------
        if (nbma .gt. nbmadi) then
            nlolim = nbma - nbmadi
            nbmax = lolimx+max(nlolim,nbmail)
            call juveca(ligret//'.LIMA', nbmax)
            call jeveuo(ligret//'.LIMA', 'E', idlima)
            call juveca(ligret//'.LITY', nbmax)
            call jeveuo(ligret//'.LITY', 'E', idlity)
        endif
!
! ---   VERIFICATION DE L'ADEQUATION DE LA TAILLE DU VECTEUR
! ---   DES POINTEURS DANS LA LISTE DE MAILLES :
!       --------------------------------------
!
! ---   NOMBRE D'AFFECTATIONS DE MAILLES :
!       --------------------------------
        zi(idapma) = zi(idapma) + 1
!
! ---   LONGUEUR DU VECTEUR LIGRET.POMA :
!       -------------------------------
        call jelira(ligret//'.POMA', 'LONMAX', lopomx, k8bid)
!
! ---   REAJUSTEMENT EVENTUEL DU VECTEUR POMA :
!       -------------------------------------
        if (zi(idapma) .ge. lopomx) then
            call juveca(ligret//'.POMA', 2*lopomx)
            call jeveuo(ligret//'.POMA', 'E', idpoma)
        endif
!
    endif
!
! --- VERIFICATION DE L'ADEQUATION DE L'AFFECTATION DES NOEUDS
! --- A LA LISTE DES NOEUDS CUMULES :
!     =============================
    if (zi(jdlima) .eq. 0 .and. nbma .eq. 1) then
!
! ---   NOMBRE DE NOEUDS DEJA AFFECTES :
!       ------------------------------
        call jelira(ligret//'.LINO', 'LONUTI', lolino, k8bid)
!
! ---   LONGUEUR DU VECTEUR LIGRET.LINO :
!       -------------------------------
        call jelira(ligret//'.LINO', 'LONMAX', lolinx, k8bid)
!
! ---   NOMBRE DE NOEUDS DISPONIBLES :
!       ----------------------------
        nbnodi = lolinx - lolino
!
! ---   REAJUSTEMENT EVENTUEL DU VECTEUR LINO :
!       -------------------------------------
        if (nbno .gt. nbnodi) then
            nlolin = nbno - nbnodi
            nbnox = lolinx+max(nlolin,nbmail)
            call juveca(ligret//'.LINO', nbnox)
            call jeveuo(ligret//'.LINO', 'E', idlino)
        endif
!
! ---   VERIFICATION DE L'ADEQUATION DE LA TAILLE DU VECTEUR
! ---   DES POINTEURS DANS LA LISTE DE NOEUDS :
!       -------------------------------------
!
! ---   NOMBRE D'AFFECTATIONS DE NOEUDS :
!       -------------------------------
        zi(idapno) = zi(idapno) + 1
!
! ---   LONGUEUR DU VECTEUR LIGRET.PONO :
!       -------------------------------
        call jelira(ligret//'.PONO', 'LONMAX', loponx, k8bid)
!
! ---   REAJUSTEMENT EVENTUEL DU VECTEUR PONO :
!       -------------------------------------
        if (zi(idapno) .gt. loponx) then
            call juveca(ligret//'.PONO', 2*loponx)
            call jeveuo(ligret//'.PONO', 'E', idpono)
        endif
!
    endif
!
! --- AFFECTATION DES MAILLES TARDIVES :
!     ================================
    if (zi(jdlima) .eq. 0 .and. nbma .eq. 1) then
!
! ---   ON INCREMENTE LE NOMBRE DE MAILLES TARDIVES :
!       -------------------------------------------
        zi(idmata) = zi(idmata) + 1
        matard = matard + 1
!
! ---   AFFECTATION DU VECTEUR DES NOEUDS DU LIGRET :
!       -------------------------------------------
        do 10 i = 1, nbno
            zi(idlino+zi(idpono+matard-1)+i-1) = zi(jdlino+i-1)
10      continue
!
        zi(idpono+matard) = zi(idpono+matard-1) + nbno
!
        call jeecra(ligret//'.LINO', 'LONUTI', zi(idpono+matard), k8bid)
!
! --- AFFECTATION DES MAILLES PHYSIQUES :
!     =================================
    else
!
! ---   AFFECTATION DU TYPE DES MAILLES :
!       -------------------------------
        if (typel .eq. ' ') then
            call jenonu(jexnom('&CATA.'//pheno(1:13)//'.MODL', modeli), imodl)
            call jeveuo(jexnum('&CATA.'//pheno, imodl), 'L', jdpm)
            call jeveuo(typmai, 'L', jdtm)
        else
            call jenonu(jexnom('&CATA.TE.NOMTE', typel), ityp)
        endif
!
!
! ---   AFFECTATION DE LA LISTE DES MAILLES CUMULEES :
!       --------------------------------------------
        do 20 i = 1, nbma
            zi(idlima+zi(idpoma+zi(idapma)-1)+i-1) = zi(jdlima+i-1)
            if (typel .eq. ' ') then
                numail = zi(jdlima+i-1)
                nutypm = zi(jdtm+numail-1)
                ityp = zi(jdpm+nutypm-1)
            else
                call jenonu(jexnom('&CATA.TE.NOMTE', typel), ityp)
            endif
!
            zi(idlity+zi(idpoma+zi(idapma)-1)+i-1) = ityp
20      continue
!
        zi(idnbma) = zi(idnbma) + nbma
!
! ---   VECTEUR DE POINTEURS DANS LE VECTEUR DES MAILLES :
!       ------------------------------------------------
        zi(idpoma+zi(idapma)) = zi(idpoma+zi(idapma)-1) + nbma
!
        call jeecra(ligret//'.LIMA', 'LONUTI', zi(idpoma+zi(idapma)), k8bid)
!
    endif
!
! --- AFFECTATION DE LA MODELISATION AU LIGRET :
!     ----------------------------------------
    zk16(idmode) = modeli
!
! --- AFFECTATION DU PHENOMENE AU LIGRET :
!     ----------------------------------
    zk16(idphen) = pheno
!
    call jedema()
!
end subroutine
