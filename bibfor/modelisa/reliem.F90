subroutine reliem(mo, ma, typem, motfaz, iocc,&
                  nbmocl, limocl, tymocl, litroz, nbtrou)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: iocc, nbmocl, nbtrou
    character(len=8) :: ma, modele
    character(len=*) :: limocl(nbmocl), tymocl(nbmocl), mo
    character(len=*) :: litroz, typem, motfaz
! ----------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
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
!     CE MODULE PERMET DE CREER UN OBJET JEVEUX CONTENANT UNE LISTE
!     DE NOMS OU NUMEROS DE MAILLES OU DE NOEUDS CORRESPONDANT AUX
!     MOTS-CLES TRANSMIS EN ARGUMENTS.
!
! IN  : MO     : NOM DU MODELE (FACULTATIF SINON : ' ')
!           SI LE NOM DU MODELE EST DONNE, ON VERIFIERA QUE LES MAILLES
!           (OU LES NOEUDS) RECUPERES FONT PARTIE DU MODELE.
!           S'ILS NE FONT PAS PARTIE DU MODELE => ALARME
! IN  : MA     : NOM DU MAILLAGE
! IN  : TYPEM  : PRECISE LE TYPE DE LISTE QUE L'ON VEUT RECUPERER
!              : 'NU_MAILLE'  : NUMEROS DE MAILLES
!              : 'NO_MAILLE'  : NOMS    DE MAILLES
!              : 'NU_NOEUD'   : NUMEROS DE NOEUDS
!              : 'NO_NOEUD'   : NOMS    DE NOEUDS
! IN  : MOTFAZ : NOM DU MOT CLE FACTEUR (OU ' ')
! IN  : IOCC   : NUMERO DE L'OCCURENCE DU MOT CLE FACTEUR
! IN  : NBMOCL : NOMBRE DE MOTS CLES A SCRUTER
!                (DIMENSION DE LIMOCL)
! IN  : LIMOCL : LISTE DES MOTS CLE A SCRUTER
! IN  : TYMOCL : LISTE DES TYPES DE MOTS CLE A SCRUTER :
!                / 'GROUP_MA'
!                / 'GROUP_NO'
!                / 'MAILLE'
!                / 'NOEUD'
!                / 'TOUT'   % TOUT:'OUI'
! IN/JXOUT : LITROZ : NOM DE L'OBJET JEVEUX QUI CONTIENDRA LA LISTE DES
!                     ENTITES (MAILLE OU NOEUD) TROUVEES
! OUT : NBTROU : NOMBRE D'ENTITES TROUVEES
! ----------------------------------------------------------------------
    character(len=24) :: litrou
    integer :: jno, jma, kno, kma, iacnex, iem, nem, numno, nno, nma, nbenc
    integer :: ibid, ient
    integer :: itrno, itrma, ima, ino, nbma, nbno, nbnoma, imo, ier
    integer :: lma, lno, itbma, itbno, iret, inoem, ntou, k, ifm, niv
    character(len=8) :: type2, oui, noent, nomgd
    character(len=16) :: motfac, motcle, typmcl, phenom
    character(len=19) :: ligrel
    character(len=24) :: karg
    integer :: iarg
    integer, pointer :: maille(:) => null()
    integer, pointer :: prnm(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
    litrou = litroz
    motfac = motfaz
    modele = mo
    call infniv(ifm, niv)
!
!     --- VERIFICATIONS PRELIMINAIRES ---
!
    if (typem .ne. 'NO_MAILLE' .and. typem .ne. 'NO_NOEUD' .and. typem .ne. 'NU_MAILLE'&
        .and. typem .ne. 'NU_NOEUD') then
        ASSERT(.false.)
    endif
!
    type2 = typem(4:)
    do imo = 1, nbmocl
        motcle = limocl(imo)
        typmcl = tymocl(imo)
        if (typmcl .eq. 'NOEUD' .or. typmcl .eq. 'GROUP_NO') then
            if (type2 .eq. 'MAILLE') then
                ASSERT(.false.)
            endif
            else if (typmcl.ne.'MAILLE' .and. typmcl.ne.'GROUP_MA' .and.&
        typmcl.ne.'TOUT') then
            ASSERT(.false.)
        endif
    end do
!
!     --- EN CAS D'EXISTENCE DE L'OBJET, ON LE DETRUIT ---
!
    call jeexin(litrou, iret)
    if (iret .ne. 0) call jedetr(litrou)
!
!     --- CREATION DES TABLEAUX DE TRAVAIL ---
!
    call dismoi('NB_MA_MAILLA', ma, 'MAILLAGE', repi=nbma)
    if (nbma .gt. 0) then
        call wkvect('&&RELIEM.INDIC_MAILLE', 'V V S', max(nbma, 1), itrma)
        if (modele .ne. ' ') call jeveuo(modele//'.MAILLE', 'L', vi=maille)
    endif
    call dismoi('NB_NO_MAILLA', ma, 'MAILLAGE', repi=nbno)
    call wkvect('&&RELIEM.INDIC_NOEUD', 'V V S', nbno, itrno)
!
    do k = 1, nbma
        zi4(itrma-1+k) = 0
    end do
    do k = 1, nbno
        zi4(itrno-1+k) = 0
    end do
!
!
!
!     --- CONSTITUTION DES LISTES DES MAILLES ET DES NOEUDS
!         PAR MARQUAGE DANS LES TABLEAUX DE TRAVAIL         ---
!
!
    do imo = 1, nbmocl
        motcle = limocl(imo)
        typmcl = tymocl(imo)
!
!        -- CAS TOUT:'OUI'
!        -----------------
        if (typmcl .eq. 'TOUT') then
            call getvtx(motfac, motcle, iocc=iocc, scal=oui, nbret=ntou)
            if (ntou .gt. 0) then
                if (type2 .eq. 'MAILLE') then
                    do k = 1, nbma
                        if (modele .ne. ' ') then
                            if (maille(k) .ne. 0) then
                                zi4(itrma-1+k) = 1
                            endif
                        else
                            zi4(itrma-1+k) = 1
                        endif
                    end do
                endif
                if (type2 .eq. 'NOEUD') then
                    do k = 1, nbno
                        zi4(itrno-1+k) = 1
                    end do
                endif
            endif
            goto 90
        endif
!
!
        call getvem(ma, typmcl, motfac, motcle, iocc,&
                    iarg, 0, karg, nem)
        nem = -nem
        if (nem .eq. 0) goto 90
        if (typmcl(1:6) .ne. 'GROUP_') then
            call wkvect('&&RELIEM.NOM_EM', 'V V K8', nem, inoem)
            call getvem(ma, typmcl, motfac, motcle, iocc,&
                        iarg, nem, zk8(inoem), nem)
        else
            call wkvect('&&RELIEM.NOM_EM', 'V V K24', nem, inoem)
            call getvem(ma, typmcl, motfac, motcle, iocc,&
                        iarg, nem, zk24(inoem), nem)
        endif
!
        do iem = 1, nem
            if (typmcl(1:6) .ne. 'GROUP_') then
                karg = zk8(inoem-1+iem)
            else
                karg = zk24(inoem-1+iem)
            endif
!
            if (typmcl .eq. 'MAILLE') then
                call jenonu(jexnom(ma//'.NOMMAI', karg), ima)
                zi4(itrma-1+ima) = 1
!
            else if (typmcl.eq.'GROUP_MA') then
                call jelira(jexnom(ma//'.GROUPEMA', karg), 'LONUTI', nma)
                call jeveuo(jexnom(ma//'.GROUPEMA', karg), 'L', kma)
!
!           -- UNE VERIFICATION PENDANT LE CHANTIER "GROUPES VIDES" :
                call jelira(jexnom(ma//'.GROUPEMA', karg), 'LONMAX', ibid)
                if (ibid .eq. 1) then
                    ASSERT(nma.le.1)
                else
                    ASSERT(nma.eq.ibid)
                endif
!
                do jma = 1, nma
                    ima = zi(kma-1+jma)
                    zi4(itrma-1+ima) = 1
                end do
!
            else if (typmcl.eq.'NOEUD') then
                call jenonu(jexnom(ma//'.NOMNOE', karg), ino)
                zi4(itrno-1+ino) = 1
!
            else if (typmcl.eq.'GROUP_NO') then
                call jelira(jexnom(ma//'.GROUPENO', karg), 'LONUTI', nno)
                call jeveuo(jexnom(ma//'.GROUPENO', karg), 'L', kno)
!
!           -- UNE VERIFICATION PENDANT LE CHANTIER "GROUPES VIDES" :
                call jelira(jexnom(ma//'.GROUPENO', karg), 'LONMAX', ibid)
                if (ibid .eq. 1) then
                    ASSERT(nno.le.1)
                else
                    ASSERT(nno.eq.ibid)
                endif
!
                do jno = 1, nno
                    ino = zi(kno-1+jno)
                    zi4(itrno-1+ino) = 1
                end do
            endif
        end do
        call jedetr('&&RELIEM.NOM_EM')
 90     continue
    end do
!
!     --- AJOUT DES NOEUDS DE LA LISTE DES MAILLES A CELLE DES NOEUDS
!
    if (type2 .eq. 'NOEUD') then
        do ima = 1, nbma
            if (zi4(itrma-1+ima) .ne. 0) then
                call jeveuo(jexnum(ma//'.CONNEX', ima), 'L', iacnex)
                call jelira(jexnum(ma//'.CONNEX', ima), 'LONMAX', nbnoma)
                do ino = 1, nbnoma
                    numno = zi(iacnex-1+ino)
                    zi4(itrno-1+numno) = 1
                end do
            endif
        end do
    endif
!
!
!     --- CREATION DE L'OBJET JEVEUX LITROU ---
    if (type2 .eq. 'MAILLE') then
!
!        --- COMPTAGE DES MAILLES ---
!
        nbtrou = 0
        do ima = 1, nbma
            if (zi4(itrma-1+ima) .ne. 0) nbtrou = nbtrou + 1
        end do
        if (nbtrou .eq. 0) goto 200
!
!
!
        if (typem(1:2) .eq. 'NU') then
            call wkvect(litrou, 'V V I', nbtrou, itbma)
!
!
!           --- RANGEMENT DES NUMEROS DE MAILLES ---
            lma = 0
            do ima = 1, nbma
                if (zi4(itrma-1+ima) .ne. 0) then
                    lma = lma + 1
                    zi(itbma-1+lma) = ima
                endif
            end do
!
        else
            call wkvect(litrou, 'V V K8', nbtrou, itbma)
!
!
!           --- RANGEMENT DES NOMS DE MAILLES ---
            lma = 0
            do ima = 1, nbma
                if (zi4(itrma-1+ima) .ne. 0) then
                    lma = lma + 1
                    call jenuno(jexnum(ma//'.NOMMAI', ima), zk8(itbma-1+ lma))
                endif
            end do
        endif
!
!
!
!       -- ON VERIFIE QUE LES MAILLES FONT PARTIE DU MODELE :
!       ----------------------------------------------------
        if (modele .ne. ' ') then
            ier = 0
            do ima = 1, nbma
                if (zi4(itrma-1+ima) .ne. 0) then
                    if (maille(ima) .eq. 0) then
                        ier = ier + 1
                        call jenuno(jexnum(ma//'.NOMMAI', ima), noent)
                        write (ifm,*) ' MAILLE : ',noent
                    endif
                endif
            end do
            if (ier .ne. 0) then
                call utmess('F', 'MODELISA6_96', sk=motfac, si=ier)
            endif
        endif
!
!
!
    else
!
!        --- COMPTAGE DES NOEUDS ---
!
        nbtrou = 0
        do ino = 1, nbno
            if (zi4(itrno-1+ino) .ne. 0) nbtrou = nbtrou + 1
        end do
        if (nbtrou .eq. 0) goto 200
!
!
!
        if (typem(1:2) .eq. 'NU') then
            call wkvect(litrou, 'V V I', nbtrou, itbno)
!
!
!           --- RANGEMENT DES NUMEROS DE NOEUDS ---
            lno = 0
            do ino = 1, nbno
                if (zi4(itrno-1+ino) .ne. 0) then
                    lno = lno + 1
                    zi(itbno-1+lno) = ino
                endif
            end do
!
        else
            call wkvect(litrou, 'V V K8', nbtrou, itbno)
!
!
!           --- RANGEMENT DES NOMS DE NOEUDS ---
            lno = 0
            do ino = 1, nbno
                if (zi4(itrno-1+ino) .ne. 0) then
                    lno = lno + 1
                    call jenuno(jexnum(ma//'.NOMNOE', ino), zk8(itbno-1+ lno))
                endif
            end do
        endif
!
!       -- ON VERIFIE QUE LES NOEUDS FONT PARTIE DU MODELE :
!       ----------------------------------------------------
        if (modele .ne. ' ') then
            call dismoi('PHENOMENE', modele, 'MODELE', repk=phenom)
            call dismoi('NOM_GD', phenom, 'PHENOMENE', repk=nomgd)
            call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nbenc)
            call dismoi('NOM_LIGREL', modele, 'MODELE', repk=ligrel)
            call jeveuo(ligrel//'.PRNM', 'L', vi=prnm)
            ier = 0
            do ino = 1, nbno
                if (zi4(itrno-1+ino) .ne. 0) then
                    do ient = 1, nbenc
                        if (prnm(nbenc*(ino-1)+ient) .ne. 0) goto 191
                    end do
!             LE NOEUD NE PORTE AUCUNE COMPOSANTE DE LA GRANDEUR
!             ASSOCIEE AU PHENOMENE
                    ier = ier + 1
                    call jenuno(jexnum(ma//'.NOMNOE', ino), noent)
                    write (ifm,*) ' NOEUD : ',noent
                endif
191             continue
            end do
            if (ier .ne. 0) then
                call utmess('F', 'MODELISA6_13', sk=motfac, si=ier)
            endif
        endif
!
    endif
!
!
!     --- DESTRUCTION DES TABLEAUX DE TRAVAIL ---
200 continue
    call jedetr('&&RELIEM.INDIC_MAILLE')
    call jedetr('&&RELIEM.INDIC_NOEUD')
!
    call jedema()
end subroutine
