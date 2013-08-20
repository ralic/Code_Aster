subroutine calicp(chargz,fonree)
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
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getvtx.h"
#include "asterc/indik8.h"
#include "asterfort/aflrch.h"
#include "asterfort/assert.h"
#include "asterfort/cocali.h"
#include "asterfort/dismoi.h"
#include "asterfort/drz12d.h"
#include "asterfort/drz13d.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/pacoap.h"
#include "asterfort/pamano.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=4) :: fonree
    character(len=8) :: charge
    character(len=*) :: chargz
! -------------------------------------------------------
!     TRAITEMENT DU MOT CLE LIAISON_COQUE DE AFFE_CHAR_MECA .
!     L'UTILISATION DE CE MOT CLE PERMET D'AFFECTER DES RELATIONS
!     LINEAIRES ENTRE DDLS TRADUISANT UN MOUVEMENT DE CORPS SOLIDE
!     ENTRE DES COUPLES DE NOEUDS DE 2 LISTES DE NOEUDS APPARTENANT
!     AU PLAN MOYEN DE 2 COQUES PERPENDICULAIRES
! -------------------------------------------------------
!  CHARGE        - IN    - K8   - : NOM DE LA SD CHARGE
!                - JXVAR -      -   LA  CHARGE EST ENRICHIE
!                                   DES RELATIONS LINEAIRES NECESSAIRES
! -------------------------------------------------------
!
!
! --------- VARIABLES LOCALES ---------------------------
    real(kind=8) :: centre(3), theta(3), t(3)
    character(len=1) :: k1bid
    character(len=2) :: typlag
    character(len=8) :: mod, k8bid, poslag
    character(len=8) :: noma
    character(len=16) :: motfac
    character(len=19) :: ligrmo
    character(len=19) :: lisrel
    character(len=24) :: lisnoe, listyp
    character(len=24) :: lisin1, lisin2, lisin3, lisin4, lisin5, lisin6
    character(len=24) :: lisin7, lisin8, lisfi1, lisfi2, lisou1, lisou2
    integer :: iarg
! --------- FIN  DECLARATIONS  VARIABLES LOCALES --------
!
!-----------------------------------------------------------------------
    integer :: i, ibid, icoupl, idlfi1, idlfi2, idlino, idlity
    integer :: idlou1, idlou2, ier, in1, indlis, ino, iocc
    integer :: iret1, iret2, jind1, jind2, jnoma, lonfi1, lonfi2
    integer :: lonli1, lonli2, lonli3, lonli4, lonli5, lonli6, lonli7
    integer :: lonli8, narl, ndimmo, nliai, nrl

    real(kind=8) :: zero
    character(len=8) :: cmp_name, nomg
    integer :: jnom, nb_cmp
    integer :: cmp_index_dx, cmp_index_dy, cmp_index_dz
    integer :: cmp_index_drx, cmp_index_dry, cmp_index_drz

    integer :: numnoe
    character(len=24) :: lisnoe2
    integer :: idlino2

!-----------------------------------------------------------------------
    call jemarq()
    lisnoe = '&&CALICP.LISTNOE'
    lisnoe2 = '&&CALICP.LISTNO2'
    listyp = '&&CALICP.LISTYP'
    charge = chargz
    typlag = '12'
    zero = 0.0d0
!
    motfac = 'LIAISON_COQUE'
    call getfac(motfac, nliai)
    if (nliai .eq. 0) goto 999
!
    do i = 1, 3
        centre(i) = zero
        theta(i) = zero
        t(i) = zero
    end do
!
! --- NOM DE LA LISTE DE RELATIONS :
!     ----------------------------
    lisrel = '&&CALICP.RLLISTE'
!
! --- NOM DES LISTES DE TRAVAIL :
!     -------------------------
    lisin1 = '&&CALICP.LISMA1'
    lisin2 = '&&CALICP.LISGMA1'
    lisin3 = '&&CALICP.LISNO1'
    lisin4 = '&&CALICP.LISGNO1'
    lisin5 = '&&CALICP.LISMA2'
    lisin6 = '&&CALICP.LISGMA2'
    lisin7 = '&&CALICP.LISNO2'
    lisin8 = '&&CALICP.LISGNO2'
    lisfi1 = '&&CALICP.LISFI1'
    lisfi2 = '&&CALICP.LISFI2'
    lisou1 = '&&CALICP.LISOU1'
    lisou2 = '&&CALICP.LISOU2'

!
! --- MODELE ASSOCIE AU LIGREL DE CHARGE :
!     ----------------------------------
    call dismoi('F', 'NOM_MODELE', charge(1:8), 'CHARGE', ibid,&
                mod, ier)
!
! ---  LIGREL DU MODELE :
!      ----------------
    ligrmo = mod(1:8)//'.MODELE'
!
! --- MAILLAGE ASSOCIE AU MODELE :
!     --------------------------
    call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
    noma = zk8(jnoma)
!
! --- DIMENSION ASSOCIEE AU MODELE :
!     ----------------------------
    call dismoi('F', 'DIM_GEOM', mod, 'MODELE', ndimmo,&
                k8bid, ier)
    if (.not.(ndimmo.eq.2.or.ndimmo.eq.3)) call u2mess('F', 'MODELISA2_6')
!
! - Information about <GRANDEUR>
!
    nomg = 'DEPL_R'
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg), 'L', jnom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nb_cmp, k8bid)
!
! - Index in DEPL_R <GRANDEUR> for DX, DY, DZ, DRX, DRY, DRZ
!
    cmp_name  = 'DX'
    cmp_index_dx = indik8(zk8(jnom), cmp_name, 1, nb_cmp)
    ASSERT(cmp_index_dx.gt.0)
    cmp_name  = 'DY'
    cmp_index_dy = indik8(zk8(jnom), cmp_name, 1, nb_cmp)
    ASSERT(cmp_index_dy.gt.0)
    cmp_name  = 'DZ'
    cmp_index_dz = indik8(zk8(jnom), cmp_name, 1, nb_cmp)
    ASSERT(cmp_index_dz.gt.0)
    cmp_name  = 'DRX'
    cmp_index_drx = indik8(zk8(jnom), cmp_name, 1, nb_cmp)
    ASSERT(cmp_index_drx.gt.0)
    cmp_name  = 'DRY'
    cmp_index_dry = indik8(zk8(jnom), cmp_name, 1, nb_cmp)
    ASSERT(cmp_index_dry.gt.0)
    cmp_name  = 'DRZ'
    cmp_index_drz = indik8(zk8(jnom), cmp_name, 1, nb_cmp)
    ASSERT(cmp_index_drz.gt.0)
!
! --- CREATION D'UN VECTEUR DE 2 TERMES K8 QUI SERONT LES NOMS DES
! --- NOEUDS A RELIER :
!     ---------------
    call wkvect(lisnoe, 'V V K8', 2, idlino)
    call wkvect(lisnoe2, 'V V I', 2, idlino2)
!
! --- CREATION D'UN VECTEUR DE 2 TERMES K8 QUI SERONT LES NOMS DES
! --- TYPES LICITES D'ELEMENTS A LA JONCTION DES COQUES, CE SONT
! --- DES SEG2 OU DES SEG3 :
!     --------------------
    call wkvect(listyp, 'V V K8', 2, idlity)
    zk8(idlity+1-1) = 'SEG2'
    zk8(idlity+2-1) = 'SEG3'
!
! --- BOUCLE SUR LES OCCURENCES DU MOT-FACTEUR LIAISON_COQUE :
!     ------------------------------------------------------
    do iocc = 1, nliai
!
        call jedetr(lisin1)
        call jedetr(lisin2)
        call jedetr(lisin3)
        call jedetr(lisin4)
        call jedetr(lisin5)
        call jedetr(lisin6)
        call jedetr(lisin7)
        call jedetr(lisin8)
        call jedetr(lisfi1)
        call jedetr(lisfi2)
        call jedetr(lisou1)
        call jedetr(lisou2)
!
! ---  ON REGARDE SI LES MULTIPLICATEURS DE LAGRANGE SONT A METTRE
! ---  APRES LES NOEUDS PHYSIQUES LIES PAR LA RELATION DANS LA MATRICE
! ---  ASSEMBLEE :
! ---  SI OUI TYPLAG = '22'
! ---  SI NON TYPLAG = '12'
!      --------------------
        call getvtx(motfac, 'NUME_LAGR', iocc, iarg, 0,&
                    k8bid, narl)
        if (narl .ne. 0) then
            call getvtx(motfac, 'NUME_LAGR', iocc, iarg, 1,&
                        poslag, nrl)
            if (poslag(1:5) .eq. 'APRES') then
                typlag = '22'
            else
                typlag = '12'
            endif
        else
            typlag = '12'
        endif
!
! ---  ACQUISITION DE LA LISTE DES NOEUDS SPECIFIEE APRES
! ---  LE MOT CLE MAILLE_1 (CETTE LISTE EST NON REDONDANTE) :
!      ----------------------------------------------------
        call pamano(motfac, 'MAILLE_1', noma, listyp, iocc,&
                    lisin1, lonli1)
!
! ---  ACQUISITION DE LA LISTE DES NOEUDS SPECIFIEE APRES
! ---  LE MOT CLE GROUP_MA_1 (CETTE LISTE EST NON REDONDANTE) :
!      -----------------------------------------------------
        call pamano(motfac, 'GROUP_MA_1', noma, listyp, iocc,&
                    lisin2, lonli2)
!
! ---  ACQUISITION DE LA LISTE DES NOEUDS SPECIFIEE APRES
! ---  LE MOT CLE NOEUD_1 (CETTE LISTE EST NON REDONDANTE) :
!      ---------------------------------------------------
        call pamano(motfac, 'NOEUD_1', noma, listyp, iocc,&
                    lisin3, lonli3)
!
! ---  ACQUISITION DE LA LISTE DES NOEUDS SPECIFIEE APRES
! ---  LE MOT CLE GROUP_NO_1 (CETTE LISTE EST NON REDONDANTE) :
!      ------------------------------------------------------
        call pamano(motfac, 'GROUP_NO_1', noma, listyp, iocc,&
                    lisin4, lonli4)
!
! ---  ACQUISITION DE LA LISTE DES NOEUDS SPECIFIEE APRES
! ---  LE MOT CLE MAILLE_2 (CETTE LISTE EST NON REDONDANTE) :
!      ----------------------------------------------------
        call pamano(motfac, 'MAILLE_2', noma, listyp, iocc,&
                    lisin5, lonli5)
!
! ---  ACQUISITION DE LA LISTE DES NOEUDS SPECIFIEE APRES
! ---  LE MOT CLE GROUP_MA_2 (CETTE LISTE EST NON REDONDANTE) :
!      -----------------------------------------------------
        call pamano(motfac, 'GROUP_MA_2', noma, listyp, iocc,&
                    lisin6, lonli6)
!
! ---  ACQUISITION DE LA LISTE DES NOEUDS SPECIFIEE APRES
! ---  LE MOT CLE NOEUD_2 (CETTE LISTE EST NON REDONDANTE) :
!      ---------------------------------------------------
        call pamano(motfac, 'NOEUD_2', noma, listyp, iocc,&
                    lisin7, lonli7)
!
! ---  ACQUISITION DE LA LISTE DES NOEUDS SPECIFIEE APRES
! ---  LE MOT CLE GROUP_NO_2 (CETTE LISTE EST NON REDONDANTE) :
!      ------------------------------------------------------
        call pamano(motfac, 'GROUP_NO_2', noma, listyp, iocc,&
                    lisin8, lonli8)
!
! ---  CONCATENATION DES LISTES DESTINEES A CONSTITUER LA PREMIERE
! ---  LISTE DE NOEUDS A METTRE EN VIS A VIS DANS LE CAS
! ---  OU ELLES EXISTENT :
!      -----------------
        if (lonli1 .ne. 0) then
            call cocali(lisfi1, lisin1, 'K8')
        endif
        if (lonli2 .ne. 0) then
            call cocali(lisfi1, lisin2, 'K8')
        endif
        if (lonli3 .ne. 0) then
            call cocali(lisfi1, lisin3, 'K8')
        endif
        if (lonli4 .ne. 0) then
            call cocali(lisfi1, lisin4, 'K8')
        endif
!
! ---  CONCATENATION DES LISTES DESTINEES A CONSTITUER LA SECONDE
! ---  LISTE DE NOEUDS A METTRE EN VIS A VIS DANS LE CAS
! ---  OU ELLES EXISTENT :
!      -----------------
        if (lonli5 .ne. 0) then
            call cocali(lisfi2, lisin5, 'K8')
        endif
        if (lonli6 .ne. 0) then
            call cocali(lisfi2, lisin6, 'K8')
        endif
        if (lonli7 .ne. 0) then
            call cocali(lisfi2, lisin7, 'K8')
        endif
        if (lonli8 .ne. 0) then
            call cocali(lisfi2, lisin8, 'K8')
        endif
!
! --- VERIFICATION DE LA CONSTITUTION DES LISTES DE NOEUDS A METTRE
! --- EN VIS A VIS :
!     ------------
        call jeexin(lisfi1, iret1)
        if (iret1 .eq. 0) then
            call u2mess('F', 'MODELISA3_3')
        endif
        call jeexin(lisfi2, iret2)
        if (iret2 .eq. 0) then
            call u2mess('F', 'MODELISA3_4')
        endif
        call jelira(lisfi1, 'LONMAX', lonfi1, k1bid)
        call jelira(lisfi2, 'LONMAX', lonfi2, k1bid)
        if (lonfi1 .eq. 0) then
            call u2mess('F', 'MODELISA3_5')
        endif
        if (lonfi2 .eq. 0) then
            call u2mess('F', 'MODELISA3_6')
        endif
!
! ---  ELIMINATION DES DOUBLONS DE LISFI1 ET LISFI2 :
!      ============================================
        call jeveuo(lisfi1, 'E', idlfi1)
        call jeveuo(lisfi2, 'E', idlfi2)
!
! ---  CREATION ET AFFECTATION D'UN TABLEAU D'INDICES DISANT POUR UN
! ---  NOEUD S'IL EST DEJA APPARU DANS LA LISTE OU NON :
!      -----------------------------------------------
!
        call jedetr('&&CALICP.INDIC1')
        call jedetr('&&CALICP.INDIC2')
!
        call wkvect('&&CALICP.INDIC1', 'V V I', lonfi1, jind1)
!
        do ino = 1, lonfi1
            do in1 = ino+1, lonfi1
                if (zk8(idlfi1+in1-1) .eq. zk8(idlfi1+ino-1)) then
                    zi(jind1+in1-1) = 1
                endif
            enddo
        enddo
!
        indlis = 0
        do ino = 1, lonfi1
            if (zi(jind1+ino-1) .eq. 0) then
                indlis = indlis + 1
                zk8(idlfi1+indlis-1) = zk8(idlfi1+ino-1)
            endif
        enddo
!
        lonfi1 = indlis
!
! ---  CREATION ET AFFECTATION D'UN TABLEAU D'INDICES DISANT POUR UN
! ---  NOEUD S'IL EST DEJA APPARU DANS LA LISTE OU NON :
!      -----------------------------------------------
        call wkvect('&&CALICP.INDIC2', 'V V I', lonfi2, jind2)
!
        do ino = 1, lonfi2
            do in1 = ino+1, lonfi2
                if (zk8(idlfi2+in1-1) .eq. zk8(idlfi2+ino-1)) then
                    zi(jind2+in1-1) = 1
                endif
            enddo
        enddo
!
        indlis = 0
        do ino = 1, lonfi2
            if (zi(jind2+ino-1) .eq. 0) then
                indlis = indlis + 1
                zk8(idlfi2+indlis-1) = zk8(idlfi2+ino-1)
            endif
        enddo
!
        lonfi2 = indlis
!
        if (lonfi1 .ne. lonfi2) then
            call u2mess('F', 'MODELISA3_7')
        endif
!
! ---  MISE EN VIS-A-VIS DES NOEUDS DES 2 LISTES DE NOEUDS LISFI1 ET
! ---  LISFI2. LES LISTES REARRANGEES SONT LISOU1 ET LISOU2 :
!      ----------------------------------------------------
        call pacoap(lisfi1, lisfi2, lonfi1, centre, theta,&
                    t, noma, lisou1, lisou2)
!
! ---  CREATION DES RELATIONS LINEAIRES TRADUISANT UN MOUVEMENT
! ---  DE CORPS SOLIDE PAR COUPLE DE NOEUDS DES NOEUDS DES LISTES
! ---  LISOU1 ET LISOU2 :
!      ----------------
        call jeveuo(lisou1, 'L', idlou1)
        call jeveuo(lisou2, 'L', idlou2)
!
        do icoupl = 1, lonfi1
            zk8(idlino+1-1) = zk8(idlou1+icoupl-1)
            zk8(idlino+2-1) = zk8(idlou2+icoupl-1)
            call jenonu(jexnom(noma//'.NOMNOE', zk8(idlou1+icoupl-1)), numnoe)
            zi(idlino2+1-1) = numnoe
            call jenonu(jexnom(noma//'.NOMNOE', zk8(idlou2+icoupl-1)), numnoe)
            zi(idlino2+2-1) = numnoe
            if (ndimmo .eq. 2) then
                call drz12d(noma, ligrmo, fonree, 2 ,lisnoe2, &
                            cmp_index_drz, typlag, lisrel)
            else if (ndimmo.eq.3) then
                call drz13d(noma, ligrmo, fonree, 2,  lisnoe2, &
                            cmp_index_dx, cmp_index_dy, cmp_index_dz, cmp_index_drx,cmp_index_dry, &
                            cmp_index_drz, typlag, lisrel)
            endif
        enddo
    end do
!
! --- AFFECTATION DE LA LISTE_RELA A LA CHARGE :
!     ----------------------------------------
    call aflrch(lisrel, charge)
!
!
! --- MENAGE
!
    call jedetr('&&CALICP.LISTNO2')
    call jedetr('&&CALICP.LISTNOE')
    call jedetr('&&CALICP.LISTYP')
    call jedetr('&&CALICP.RLLISTE')
    call jedetr('&&CALICP.LISMA1')
    call jedetr('&&CALICP.LISGMA1')
    call jedetr('&&CALICP.LISNO1')
    call jedetr('&&CALICP.LISGNO1')
    call jedetr('&&CALICP.LISMA2')
    call jedetr('&&CALICP.LISGMA2')
    call jedetr('&&CALICP.LISNO2')
    call jedetr('&&CALICP.LISGNO2')
    call jedetr('&&CALICP.LISFI1')
    call jedetr('&&CALICP.LISFI2')
    call jedetr('&&CALICP.LISOU1')
    call jedetr('&&CALICP.LISOU2')
    call jedetr('&&CALICP.INDIC1')
    call jedetr('&&CALICP.INDIC2')
!
999 continue
    call jedema()
end subroutine
