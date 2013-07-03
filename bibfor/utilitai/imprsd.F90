subroutine imprsd(typesd, nomsd, ific, titre)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/celces.h"
#include "asterfort/cesimp.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnsimp.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/matimp.h"
#include "asterfort/tbimpr.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    character(len=*) :: typesd, nomsd, titre
    integer :: ific
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
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!  BUT : IMPRIMER UNE STRUCTURE DE DONNEE DONT ON CONNAIT LE TYPE
!
!
! TYPESD  IN K*   : TYPE DE LA STRUCTURE DE DONNEE A IMPRIMER
!                      'CHAMP' :   /CHAM_NO/CHAM_ELEM/RESUELEM/CARTE
!                                  /CHAM_NO_S/CHAM_ELEM_S
!                      'MATRICE' : MATR_ASSE/MATR_GENE
! NOMSD   IN K*  : NOM DE LA STRUCTURE DE DONNEES A IMPRIMER
! IFIC    IN I   : NUMERO LOGIQUE DU FICHIER ASCII POUR L'IMPRESSION
! TITRE   IN K*  : CHAINE DE CARACTERES IMPRIMEE EN TETE
!
! ----------------------------------------------------------------------
!
    integer :: ibid, i1, i2, i3, i4, i5, i6, ib
    integer :: j1, j2, j3, k, npara
    character(len=16) :: typ2sd
    character(len=19) :: ch, chs, matr
    character(len=17) :: table
!
! -DEB------------------------------------------------------------------
!
    call jemarq()
    typ2sd = typesd
!
    call assert((ific.ne.0) .and. (ific.le.100))
    ibid = 0
!
!     1. ECRITURE DU TITRE :
!     ----------------------
    write (ific,*) ' '
    write (ific,*) '-----------------------------------------------'
    write (ific,*) titre
!
!
!     2. APPEL A LA BONNE ROUTINE :
!     ------------------------------
!
    if ((typ2sd.eq.'CHAMP') .or. (typ2sd.eq.'CHAMP_GD') .or. (typ2sd.eq.'CHAMP_S')) then
!     ------------------------------------
        ch = nomsd
        chs = '&&IMPRSD.CHS'
!
        call exisd('CHAM_NO_S', ch, i1)
        call exisd('CHAM_ELEM_S', ch, i2)
        call exisd('CHAM_NO', ch, i3)
        call exisd('CHAM_ELEM', ch, i4)
        call exisd('CARTE', ch, i5)
        call exisd('RESUELEM', ch, i6)
!
!
        if (i1 .gt. 0) call cnsimp(ch, ific)
        if (i2 .gt. 0) call cesimp(ch, ific, ibid, ibid)
!
        if (i3 .gt. 0) then
            call cnocns(ch, 'V', chs)
            call cnsimp(chs, ific)
            call detrsd('CHAM_NO_S', chs)
        endif
!
        if (i4 .gt. 0) then
            call celces(ch, 'V', chs)
            call cesimp(chs, ific, ibid, ibid)
            call detrsd('CHAM_ELEM_S', chs)
        endif
!
        if (i5 .gt. 0) then
            call carces(ch, 'ELEM', ' ', 'V', chs,&
                        'A', ib)
            call cesimp(chs, ific, ibid, ibid)
            call detrsd('CHAM_ELEM_S', chs)
        endif
!
        if (i6 .gt. 0) write (ific,*) 'TYPE : RESUELEM NON TRAITE.'
!
!
    else if (typ2sd.eq.'TABLE') then
!     --------------------------------------
        table=nomsd
        call jeveuo(table//'  .TBNP', 'L', j1)
        call jeveuo(table//'  .TBLP', 'L', j2)
        npara=zi(j1)
        call wkvect('&&IMPRSD.LIPARA', 'V V K16', npara, j3)
        do 1, k=1,npara
        zk16(j3-1+k)=zk24(j2-1+4*(k-1)+1)
 1      continue
        call tbimpr(table, 'ASTER', ific, npara, zk16(j3),&
                    0, '1PE12.5')
        call jedetr('&&IMPRSD.LIPARA')
!
!
    else if (typ2sd.eq.'MATRICE') then
!     --------------------------------------
        matr=nomsd
        call matimp(matr, ific, 'ASTER')
!
!
    else
!     --------------------------------------
        call u2mesk('F', 'UTILITAI_47', 1, typ2sd)
    endif
!
    call jedema()
end subroutine
