subroutine op0001()
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
!
!-----------------------------------------------------------------------
!           operateur    LIRE_MAILLAGE
!-----------------------------------------------------------------------
!
!       COODSC          NOM DE L OBJET CHAMP DE GEOMETRIE (DESCRIPTEUR)
!       COOREF          NOM DE L OBJET CHAMP DE GEOMETRIE (NOM MAILLAGE)
!       GRPNOE          NOM DE L OBJET GROUPE NOEUDS
!       GRPMAI          NOM DE L OBJET GROUPE MAILLES
!       NOMMAI          NOM DE L OBJET REPERTOIRE DES MAILLES
!       NOMNOE          NOM DE L OBJET REPERTOIRE DES NOEUDS
!       TITRE           NOM DE L OBJET TITRE
!
!-----------------------------------------------------------------------
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/cargeo.h"
#include "asterfort/chckma.h"
#include "asterfort/getvem.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/infoma.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lrmast.h"
#include "asterfort/lrmhdf.h"
#include "asterfort/lxlgut.h"
#include "asterfort/mavegr.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"

!
! ----- DECLARATIONS
!
    integer :: i, iaux, niv, ifl, ifm, ibid
    integer :: nbnoeu, nbmail, nbcoor, nbcgrm
    integer :: iret, infmed
    character(len=8) :: nomu, fmt, veri
    character(len=16) :: concep,cmd
    character(len=24) :: vecgrm
    character(len=64) :: nomamd
    real(kind=8) :: dtol
    integer :: ilng
    character(len=80), pointer :: tgrm(:) => null()
    integer, pointer :: dime(:) => null()

!---------------------------------------------------------------------------------------
    call jemarq()
    vecgrm = '&&OP0001.VECGRM'
!
! --- RECUPERATION DES ARGUMENTS  DE LA COMMANDE
!
    ifl = 0
    call infmaj()
    call infniv(ifm, niv)
!
    call getres(nomu, concep, cmd)
!
    call getvis(' ', 'UNITE', scal=ifl, nbret=iaux)
!
    call getvtx(' ', 'FORMAT', scal=fmt, nbret=iaux)
!
    if (fmt(1:3) .eq. 'MED') then
        call getvtx(' ', 'NOM_MED', scal=nomamd, nbret=iaux)
        if (iaux .eq. 0) then
!                   12345678901234567890123456789012
            nomamd = ' '
        endif
        call getvis(' ', 'INFO_MED', scal=infmed, nbret=iaux)
!
!   --- LECTURE DES CORRESPONDANCES NOM MED - NOM ASTER
!
        call getfac('RENOMME', nbcgrm)
        if (nbcgrm .gt. 0) then
            call wkvect(vecgrm, 'V V K80', nbcgrm*2, vk80=tgrm)
            do 100 i = 1, nbcgrm
                call getvtx('RENOMME', 'NOM_MED', iocc=i, scal=tgrm(i*2-1), nbret=ibid)
                call getvtx('RENOMME', 'NOM', iocc=i, scal=tgrm(i*2), nbret=ibid)
                ilng = lxlgut(tgrm(i*2))
                ASSERT(ilng.gt.0 .and. ilng.le.8)
100          continue
        endif
!
    endif
!
! --- LECTURE DU MAILLAGE AU FORMAT ASTER :
!     -----------------------------------
    if (fmt(1:5) .eq. 'ASTER') then
        call lrmast(nomu, ifm, ifl, nbnoeu, nbmail,&
                    nbcoor)
!
! --- LECTURE DU MAILLAGE AU FORMAT MED :
!     ---------------------------------
    else if (fmt(1:3) .eq. 'MED') then
        call lrmhdf(nomamd, nomu, ifm, ifl, niv,&
                    infmed, nbnoeu, nbmail, nbcoor, vecgrm,&
                    nbcgrm)
    endif


! --- SUPPRESSION DES GROUPES DE NOEUDS OU MAILLES DE NOM ' ' :
!     -------------------------------------------------------
    call mavegr(nomu)


! --- CREATION DE L'OBJET .DIME :
!     -------------------------
    call wkvect(nomu//'.DIME', 'G V I', 6, vi=dime)
    dime(1)= nbnoeu
    dime(3)= nbmail
    dime(6)= nbcoor


! --- CARACTERISTIQUES GEOMETRIQUES :
!     -----------------------------
    call cargeo(nomu)


! --- PHASE DE VERIFICATION DU MAILLAGE :
!     ---------------------------------
    call getvtx('VERI_MAIL', 'VERIF', iocc=1, scal=veri, nbret=iret)
    if (veri .eq. 'OUI') then
        call getvr8('VERI_MAIL', 'APLAT', iocc=1, scal=dtol, nbret=iret)
        call chckma(nomu, dtol)
    else
        call utmess('A', 'MODELISA5_49')
    endif


!     IMPRESSIONS DU MOT CLE INFO :
!     ---------------------------
    call infoma(nomu)
!
    call jedema()
end subroutine
