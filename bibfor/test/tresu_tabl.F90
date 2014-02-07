subroutine tresu_tabl(nomta, para, typtes, typres, tbtxt,&
                      refi, refr, refc, epsi, crit,&
                      ific, llab, ssigne, ignore, compare)
    implicit none
#include "jeveux.h"
#include "asterc/ismaem.h"
#include "asterc/r8maem.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbexip.h"
#include "asterfort/tresu_print_all.h"
#include "asterfort/utmess.h"
    character(len=*), intent(in) :: nomta
    character(len=*), intent(in) :: para
    character(len=8), intent(in) :: typtes
    character(len=*), intent(in) :: typres
    character(len=16), intent(in) :: tbtxt(2)
    integer, intent(in) :: refi
    real(kind=8), intent(in) :: refr
    complex(kind=8), intent(in) :: refc
    real(kind=8), intent(in) :: epsi
    character(len=*), intent(in) :: crit
    integer, intent(in) :: ific
    logical, intent(in) :: llab
    character(len=*), intent(in) :: ssigne
    logical, intent(in), optional :: ignore
    real(kind=8), intent(in), optional :: compare
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
! IN  : NOMTA  : NOM DE LA STRUCTURE "TABLE".
! IN  : PARA   : PARAMETRE A CHERCHER
! IN  : TYPTES : TYPE DE TEST A EFFECTUER SUR LE CHAMP
! IN  : TBTXT  : (1)=REFERENCE, (2)=LEGENDE
! IN  : REFI   : VALEUR REELLE ENTIERE ATTENDUE
! IN  : REFR   : VALEUR REELLE ATTENDUE
! IN  : REFC   : VALEUR COMPLEXE ATTENDUE
! IN  : CRIT   : 'RELATIF' OU 'ABSOLU'(PRECISION RELATIVE OU ABSOLUE).
! IN  : EPSI   : PRECISION ESPEREE
! IN  : IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
! IN  : LLAB   : FLAG D IMPRESSION DES LABELS
! OUT : IMPRESSION SUR LISTING
! ----------------------------------------------------------------------
    integer :: vali, jvale, jvall, nblign, nbpara, i, jtbnp, jtblp, ipar
    real(kind=8) :: valr
    complex(kind=8) :: valc
    logical :: exist
    character(len=1) :: typrez
    character(len=4) :: type
    character(len=19) :: nomtab
    character(len=24) :: inpar
    character(len=24) :: valk(2)
    logical :: skip
    real(kind=8) :: ordgrd
!     ------------------------------------------------------------------
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
    call jemarq()
!
    nomtab = nomta
    inpar = para
    typrez = typres(1:1)
!
    call tbexip(nomta, para, exist, type)
!
    if (.not. exist) then
        valk(1) = para
        call utmess('F', 'CALCULEL6_85', sk=valk(1))
    endif
!
    if (type(1:1) .ne. typrez) then
        write(ific,*) 'NOOK '
        valk(1) = type
        valk(2) = typrez
        call utmess('A', 'CALCULEL5_11', nk=2, valk=valk)
        goto 9999
    endif
!
    call jeveuo(nomtab//'.TBNP', 'L', jtbnp)
    nbpara = zi(jtbnp )
    nblign = zi(jtbnp+1)
!
    call jeveuo(nomtab//'.TBLP', 'L', jtblp)
    do 10 ipar = 1, nbpara
        if (inpar .eq. zk24(jtblp+4*(ipar-1))) goto 12
10  end do
12  continue
    call jeveuo(zk24(jtblp+4*(ipar-1)+2), 'L', jvale)
    call jeveuo(zk24(jtblp+4*(ipar-1)+3), 'L', jvall)
!
    if (type .eq. 'I') then
        if (typtes .eq. 'SOMM_ABS') then
            vali = 0
            do 100 i = 1, nblign
                if (zi(jvall+i-1) .eq. 1) vali = vali+abs( zi(jvale+i-1) )
100          continue
        else if (typtes .eq. 'SOMM') then
            vali = 0
            do 102 i = 1, nblign
                if (zi(jvall+i-1) .eq. 1) vali = vali + zi(jvale+i-1)
102          continue
        else if (typtes .eq. 'MAX') then
            vali = -ismaem()
            do 104 i = 1, nblign
                if (zi(jvall+i-1) .eq. 1) vali = max( vali,zi(jvale+i-1) )
104          continue
        else if (typtes .eq. 'MIN') then
            vali = ismaem()
            do 106 i = 1, nblign
                if (zi(jvall+i-1) .eq. 1) vali = min( vali,zi(jvale+i-1) )
106          continue
        else
            write(ific,*) 'NOOK '
            call utmess('A', 'CALCULEL5_12')
            goto 9999
        endif
    else if (type .eq. 'R') then
        if (typtes .eq. 'SOMM_ABS') then
            valr = 0.d0
            do 200 i = 1, nblign
                if (zi(jvall+i-1) .eq. 1) valr = valr+abs( zr(jvale+i-1) )
200          continue
        else if (typtes .eq. 'SOMM') then
            valr = 0.d0
            do 202 i = 1, nblign
                if (zi(jvall+i-1) .eq. 1) valr = valr + zr(jvale+i-1)
202          continue
        else if (typtes .eq. 'MAX') then
            valr = -r8maem()
            do 204 i = 1, nblign
                if (zi(jvall+i-1) .eq. 1) valr = max( valr,zr(jvale+i-1) )
204          continue
        else if (typtes .eq. 'MIN') then
            valr = r8maem()
            do 206 i = 1, nblign
                if (zi(jvall+i-1) .eq. 1) valr = min( valr,zr(jvale+i-1) )
206          continue
        else
            write(ific,*) 'NOOK '
            call utmess('A', 'CALCULEL5_12')
            goto 9999
        endif
    else if (type .eq. 'C') then
        valc = ( 0.d0 , 0.d0 )
        if (typtes .eq. 'SOMM_ABS') then
            do 300 i = 1, nblign
                if (zi(jvall+i-1) .eq. 1) valc = valc+abs( zc(jvale+i-1) )
300          continue
        else if (typtes .eq. 'SOMM') then
            do 302 i = 1, nblign
                if (zi(jvall+i-1) .eq. 1) valc = valc + zc(jvale+i-1)
302          continue
        else
            write(ific,*) 'NOOK '
            call utmess('A', 'CALCULEL5_12')
            goto 9999
        endif
    endif
!
    call tresu_print_all(tbtxt(1), tbtxt(2), llab, typres, 1, &
                crit, epsi, ssigne, [refr], valr, &
                [refi], vali, [refc], valc, ignore=skip, &
                compare=ordgrd)
!
9999  continue
!
    call jedema()
end subroutine
