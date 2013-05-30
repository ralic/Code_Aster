subroutine calinn(prefiz, nomaz, motfaz, iocc, lisi1z,&
                  lonli1, lisi2z, lonli2, modz)
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
    implicit       none
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/getvem.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/pacoap.h'
    include 'asterfort/pacoje.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    character(len=*) :: motfaz, prefiz, nomaz, lisi1z, lisi2z, modz
    integer :: iocc
! ---------------------------------------------------------------------
!
!     BUT : CREER LA STRUCTURE INTERMEDIAIRE PRFEIXEE PAR PREFIX
!           DESCRIVANT LES COUPLES DE NOEUDS EN REGARD AVEC
!           UN VIS A VIS PAR LISTE DE NOEUDS
!     PREFIX.CONI : NUMERO DE NOEUDS EN REGARD
!     CONI : OJB G V I DIM = 2 * NBCOUPLE + 1
!            CONI(1) = NBCOUPLE (BOUCLE SUR I <= NBCOUPLE)
!            CONI(1+2*(I-1)+1) = NUM1 DU NOEUD 1
!            CONI(1+2*(I-1)+2) = NUM2 DU NOEUD 2
!
!     CONR : NORMALES EN CES NOEUDS ET JEU INITIAL
!           CONR CONTIENT LES COMPOSANTES DU VECTEUR NORMAL
!           POUR CHAQUE NOEUD EN VIS-A-VIS DONNE PAR
!           LA S.D. CONI.
!           CONR EST A L'INSTAR DE CONI UNE COLLECTION
!           ET C'EST L'OBJET D'INDICE IOCC DE CETTE COLLECTION
!           QUI EST CREE ET AFFECTE.
!
!    CONR : OJB BASE V R DIM = 12 * NBCOUPLE EN 2D
!                              22 * NBCOUPLE EN 3D
!                      I = 1, NBCOUPLE ,  J = 1, 3
!                      CONR( (2*NDIM+1)*(I-1)+J      )  = NORM1(J)
!                      CONR( (2*NDIM+1)*(I-1)+J+NDIM )  = NORM2(J)
!                      CONR( (2*NDIM+1)*I            )  = JEU
!
! IN  PREFIZ K*(*) :  NOM UTILISATEUR DU CONCEPT DE CHARGE
!                   OU PREFIXE DE L' OJB .CONI ET .CONR (EVENTUELLEMENT)
! IN  NOMAZ   K*(*): NOM DU MAILLAGE
! IN  MOTFAC  K16  : MOT CLE FACTEUR A TRAITER
! IN  IOCC    I    : SI >0 ON TRAITE L'OCCURENCE IOCC DE MOTFAC
!                    SI <0 OU =0 ERREUR FATALE
! IN  LISI1Z  K*(*): NOM DE LA PREMIERE LISTE DE NOEUDS
! IN  LONLI1  I    : LONGUEUR DE LA PREMIERE LISTE
! IN  LISI2Z  K*(*): NOM DE LA SECONDE LISTE DE NOEUDS
! IN  LONLI2  I    : LONGUEUR DE LA SECONDE LISTE
! ---------------------------------------------------------------------
!
!
!
    integer :: i, idconi, idlou1, idlou2, ier, ino1, ino2, lonli1, lonli2
    integer :: lonlis, nbma1, nbno1, n1, ndim, ng1, ngm1, nlino, no, nr, nt
    integer :: n2, n3, n4, n5, n6, n7, n8, vali(2)
    real(kind=8) :: centre(3), theta(3), t(3)
    logical :: dnor
    character(len=8) :: k8bid, ddl1, ddl2, noma, mod
    character(len=8) :: nom1, nom2
    character(len=24) :: valk(2)
    character(len=16) :: motfac
    character(len=19) :: pref19
    character(len=24) :: coni, conr, noeuma
    character(len=24) :: prefix, lisin1, lisin2, lisou1, lisou2
    integer :: iarg
!
! ---------------------------------------------------------------------
! --- DEBUT
!
    call jemarq()
!
    prefix = prefiz
    noma = nomaz
    mod = modz
    motfac = motfaz
    pref19 = prefix(1:19)
    lisin1 = lisi1z
    lisin2 = lisi2z
    dnor = .false.
!
    noeuma = noma//'.NOMNOE'
!
    if (motfac .ne. 'LIAISON_GROUP') then
        call u2mess('F', 'MODELISA2_62')
    endif
!
    call getvem(noma, 'GROUP_NO', motfac, 'GROUP_NO_1', iocc,&
                iarg, 0, k8bid, ng1)
    if (ng1 .eq. 0) then
        call getvem(noma, 'NOEUD', motfac, 'NOEUD_1', iocc,&
                    iarg, 0, k8bid, nbno1)
        if (nbno1 .eq. 0) then
            call getvem(noma, 'GROUP_MA', motfac, 'GROUP_MA_1', iocc,&
                        iarg, 0, k8bid, ngm1)
            if (ngm1 .eq. 0) then
                call getvem(noma, 'MAILLE', motfac, 'MAILLE_1', iocc,&
                            iarg, 0, k8bid, nbma1)
                if (nbma1 .eq. 0) goto 999
            endif
        endif
    endif
!
    if (iocc .le. 0) then
        call assert(.false.)
    endif
!
    call getfac(motfac, nlino)
    coni = pref19//'.CONI'
    conr = pref19//'.CONR'
    if ((nlino.eq.0) .or. (iocc.gt.nlino)) goto 999
!
! --- LECTURE DE L'ISOMETRIE DE TRANSFORMATION SI ELLE EXISTE
!
    do 10 i = 1, 3
        t(i) = 0.0d0
        theta(i) = 0.0d0
        centre(i) = 0.0d0
10  continue
!
    call getvr8(motfac, 'TRAN', iocc, iarg, 3,&
                t, nt)
    if (nt .lt. 0) then
        call u2mesk('F', 'MODELISA3_9', 1, motfac)
    endif
!
    call getvr8(motfac, 'ANGL_NAUT', iocc, iarg, 3,&
                theta, nr)
    if (nr .lt. 0) then
        call u2mesk('F', 'MODELISA3_10', 1, motfac)
    endif
!
    call getvr8(motfac, 'CENTRE', iocc, iarg, 3,&
                centre, no)
    if (no .lt. 0) then
        call u2mesk('F', 'MODELISA3_11', 1, motfac)
    endif
!
    lisou1 = '&&CALINN.LISOU1'
    lisou2 = '&&CALINN.LISOU2'
!
! ---    LES 2 LISTES DOIVENT AVOIR LA MEME LONGUEUR
!
    if (lonli1 .ne. lonli2) then
        nom1 = '        '
        nom2 = '        '
        n1 = 0
        n2 = 0
        n3 = 0
        n4 = 0
        n5 = 0
        n6 = 0
        n7 = 0
        n8 = 0
        call getvtx(motfac, 'GROUP_NO_1', iocc, iarg, 1,&
                    nom1, n1)
        if (n1 .gt. 0) valk(1)='GROUP_NO_1'
        call getvtx(motfac, 'NOEUD_1', iocc, iarg, 1,&
                    nom1, n2)
        if (n2 .gt. 0) valk(1)='NOEUD_1   '
        call getvtx(motfac, 'GROUP_MA_1', iocc, iarg, 1,&
                    nom1, n3)
        if (n3 .gt. 0) valk(1)='GROUP_MA_1'
        call getvtx(motfac, 'MAILLE_1', iocc, iarg, 1,&
                    nom1, n4)
        if (n4 .gt. 0) valk(1)='MAILLE_1  '
!
        call getvtx(motfac, 'GROUP_NO_2', iocc, iarg, 1,&
                    nom2, n5)
        if (n5 .gt. 0) valk(2)='GROUP_NO_2'
        call getvtx(motfac, 'NOEUD_2', iocc, iarg, 1,&
                    nom2, n6)
        if (n6 .gt. 0) valk(2)='NOEUD_2   '
        call getvtx(motfac, 'GROUP_MA_2', iocc, iarg, 1,&
                    nom2, n7)
        if (n7 .gt. 0) valk(2)='GROUP_MA_2'
        call getvtx(motfac, 'MAILLE_2', iocc, iarg, 1,&
                    nom2, n8)
        if (n8 .gt. 0) valk(2)='MAILLE_2  '
!
        vali(1)= lonli1
        vali(2)= lonli2
        call u2mesg('F', 'MODELISA3_12', 2, valk, 2,&
                    vali, 0, 0.d0)
!
    endif
!
! ---    MISE EN VIS-A-VIS DES NOEUDS DES LISTES LISIN1 ET LISIN2
! ---    LES LISTES REARRANGEES SONT LISOU1 ET LISOU2
!
    call pacoap(lisin1, lisin2, lonli1, centre, theta,&
                t, noma, lisou1, lisou2)
!
    call jeveuo(lisou1, 'L', idlou1)
    call jeveuo(lisou2, 'L', idlou2)
!
    lonlis = lonli1
!
    call jecroc(jexnum(coni, iocc))
    call jeecra(jexnum(coni, iocc), 'LONMAX', 2*lonlis+1, ' ')
    call jeveuo(jexnum(coni, iocc), 'E', idconi)
!
    zi(idconi) = lonlis
!
    do 20 i = 1, lonlis
        call jenonu(jexnom(noeuma, zk8(idlou1+i-1)), ino1)
        call jenonu(jexnom(noeuma, zk8(idlou2+i-1)), ino2)
        zi(idconi+2*(i-1)+1) = ino1
        zi(idconi+2*(i-1)+2) = ino2
20  end do
!
! --- CONSTITUTION DE LA S.D. CONR CONTENANT LES NORMALES
! --- AUX NOEUDS
!
    ddl1 = ' '
    call getvtx(motfac, 'DDL_1', iocc, iarg, 1,&
                ddl1, n1)
!
    ddl2 = ' '
    call getvtx(motfac, 'DDL_2', iocc, iarg, 1,&
                ddl2, n1)
!
    if (ddl1 .eq. 'DNOR' .or. ddl2 .eq. 'DNOR') then
        dnor = .true.
    endif
!
    if (dnor) then
        call dismoi('F', 'DIM_GEOM', mod, 'MODELE', ndim,&
                    k8bid, ier)
        call pacoje(coni, iocc, motfac, noma, conr,&
                    ndim)
    endif
!
! --- MENAGE
!
    call jedetr(lisin1)
    call jedetr(lisin2)
    call jedetr(lisou1)
    call jedetr(lisou2)
!
999  continue
! FIN -----------------------------------------------------------------
    call jedema()
end subroutine
