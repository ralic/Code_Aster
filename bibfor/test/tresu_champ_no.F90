subroutine tresu_champ_no(cham19, nonoeu, nocmp, nbref, tbtxt,&
                          refi, refr, refc, typres, epsi,&
                          crit, ific, llab, ssigne, ignore,&
                          compare)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/tresu_print_all.h"
#include "asterfort/utmess.h"
!
    character(len=19), intent(in) :: cham19
    character(len=33), intent(in) :: nonoeu
    character(len=8), intent(in) :: nocmp
    integer, intent(in) :: nbref
    character(len=16), intent(in) :: tbtxt(2)
    integer, intent(in) :: refi(nbref)
    real(kind=8), intent(in) :: refr(nbref)
    complex(kind=8), intent(in) :: refc(nbref)
    character(len=1), intent(in) :: typres
    real(kind=8), intent(in) :: epsi
    character(len=*), intent(in) :: crit
    integer, intent(in) :: ific
    aster_logical, intent(in) :: llab
    character(len=*), intent(in) :: ssigne
    aster_logical, intent(in), optional :: ignore
    real(kind=8), intent(in), optional :: compare
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ENTREES:
!        CHAM19 : NOM DU CHAM_NO DONT ON DESIRE VERIFIER 1 COMPOSANTE
!        NONOEU : NOM DU NOEUD A TESTER
!        NOCMP  : NOM DU DDL A TESTER SUR LE NOEUD NONOEU
!        TBTXT  : (1)=REFERENCE, (2)=LEGENDE
!        NBREF  : NOMBRE DE VALEURS DE REFERENCE
!        REFR   : VALEUR REELLE ATTENDUE SUR LE DDL DU NOEUD
!        REFC   : VALEUR COMPLEXE ATTENDUE SUR LE DDL DU NOEUD
!        CRIT   : 'RELATIF' OU 'ABSOLU'(PRECISION RELATIVE OU ABSOLUE).
!        EPSI   : PRECISION ESPEREE
!        IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
!        LLAB   : AFFICHAGE DES LABELS
!     SORTIES:
!      LISTING ...
! ----------------------------------------------------------------------
!     FONCTIONS EXTERNES:
!     VARIABLES LOCALES:
    character(len=8) :: nogd
    character(len=1) :: type
    integer :: gd, iadg, vali
    real(kind=8) :: valr
    complex(kind=8) :: valc
    character(len=4) :: testok
    character(len=8) :: nomma
    character(len=19) :: prchno, valk(3)
    character(len=24) :: nolili
    aster_logical :: skip
    real(kind=8) :: ordgrd
!
!-----------------------------------------------------------------------
    integer :: iadesc, iancmp, ianueq, iaprno, iarefe, iavale, ibid
    integer :: icmp, idecal, iicmp, ino, ival, ncmp
    integer :: ncmpmx, nec, num
!-----------------------------------------------------------------------
    call jemarq()
    testok = 'NOOK'
!
    skip = .false.
    if (present(ignore)) then
        skip = ignore
    endif
!
    ordgrd = 1.d0
    if (present(compare)) then
        ordgrd = compare
    endif
!
    call dismoi('NOM_GD', cham19, 'CHAM_NO', repk=nogd)
!
    call jeveuo(cham19//'.REFE', 'L', iarefe)
    nomma = zk24(iarefe-1+1)(1:8)
    prchno = zk24(iarefe-1+2)(1:19)
!
    call jelira(cham19//'.VALE', 'TYPE', cval=type)
    if (type .ne. typres) then
        valk(1) = cham19
        valk(2) = type
        valk(3) = typres
        call utmess('F', 'CALCULEL6_89', nk=3, valk=valk)
    else if (type.ne.'R' .and. type.ne.'C') then
        valk(1) = type
        call utmess('F', 'CALCULEL6_90', sk=valk(1))
    endif
    call jeveuo(cham19//'.VALE', 'L', iavale)
!
    call jeveuo(cham19//'.DESC', 'L', iadesc)
    gd = zi(iadesc-1+1)
    num = zi(iadesc-1+2)
    nec = nbec(gd)
!
!     -- ON RECHERCHE LE NUMERO CORRESPONDANT A NOCMP:
    call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', iancmp)
    icmp = indik8(zk8(iancmp),nocmp,1,ncmpmx)
    if (icmp .eq. 0) then
        valk(1) = nocmp
        valk(2) = nogd
        call utmess('F', 'CALCULEL6_91', nk=2, valk=valk)
    endif
!
!        -- RECUPERATION DU NUMERO DU NOEUD:
    call jenonu(jexnom(nomma//'.NOMNOE', nonoeu(1:8)), ino)
    if (ino .eq. 0) then
        valk(1) =nonoeu(1:8)
        call utmess('F', 'CALCULEL6_92', sk=valk(1))
    endif
!
!     --SI LE CHAMP EST A REPRESENTATION CONSTANTE:
!
    if (num .lt. 0) then
        ncmp = -num
!
!        -- ON COMPTE LES CMP PRESENTES SUR LE NOEUD AVANT ICMP: (+1)
        idecal = 0
        do iicmp = 1, icmp
            if (exisdg(zi(iadesc+2),iicmp)) idecal = idecal + 1
        end do
!
        if (exisdg(zi(iadesc+2),icmp)) then
            if (type .eq. 'R') then
                valr = zr(iavale-1+(ino-1)*ncmp+idecal)
            else if (type .eq. 'I') then
                vali = zi(iavale-1+(ino-1)*ncmp+idecal)
            else if (type .eq. 'C') then
                valc = zc(iavale-1+(ino-1)*ncmp+idecal)
            endif
            call tresu_print_all(tbtxt(1), tbtxt(2), llab, type, nbref,&
                                 crit, epsi, ssigne, refr, valr,&
                                 refi, vali, refc, valc, ignore=skip,&
                                 compare=ordgrd)
        else
            call utmess('F', 'CALCULEL6_93')
        endif
    else
!        --SI LE CHAMP EST DECRIT PAR 1 "PRNO":
!
        call jenuno(jexnum(prchno//'.LILI', 1), nolili)
        call jelira(jexnum(prchno//'.PRNO', 1), 'LONMAX', ibid)
        if (ibid .eq. 0) then
            write (ific,*) testok,' : 2'
            goto 999
        endif
        call jeveuo(jexnum(prchno//'.PRNO', 1), 'L', iaprno)
        call jeveuo(prchno//'.NUEQ', 'L', ianueq)
!
!        IVAL : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
!        NCMP : NOMBRE DE COMPOSANTES PRESENTES SUR LE NOEUD
!        IADG : DEBUT DU DESCRIPTEUR GRANDEUR DU NOEUD INO
        ival = zi(iaprno-1+ (ino-1)* (nec+2)+1)
        ncmp = zi(iaprno-1+ (ino-1)* (nec+2)+2)
        iadg = iaprno - 1 + (ino-1)* (nec+2) + 3
        if (ncmp .eq. 0) then
            write (ific,*) testok,' : 3'
            goto 999
        endif
!
!        -- ON COMPTE LES CMP PRESENTES SUR LE NOEUD AVANT ICMP:
        idecal = 0
        do iicmp = 1, icmp
            if (exisdg(zi(iadg),iicmp)) idecal = idecal + 1
        end do
!
        if (exisdg(zi(iadg),icmp)) then
            if (type .eq. 'R') then
                valr = zr(iavale-1+zi(ianueq-1+ival-1+idecal))
            else if (type .eq. 'I') then
                vali = zi(iavale-1+zi(ianueq-1+ival-1+idecal))
            else if (type .eq. 'C') then
                valc = zc(iavale-1+zi(ianueq-1+ival-1+idecal))
            endif
            call tresu_print_all(tbtxt(1), tbtxt(2), llab, type, nbref,&
                                 crit, epsi, ssigne, refr, valr,&
                                 refi, vali, refc, valc, ignore=skip,&
                                 compare=ordgrd)
        else
            call utmess('F', 'CALCULEL6_93')
        endif
    endif
999 continue
    call jedema()
end subroutine
