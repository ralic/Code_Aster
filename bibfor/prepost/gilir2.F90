subroutine gilir2(nfic, niv, ndim, nbobo)
! aslint: disable=
    implicit none
#include "jeveux.h"
!
#include "asterfort/gicnx2.h"
#include "asterfort/gidoma.h"
#include "asterfort/gilig0.h"
#include "asterfort/gilig1.h"
#include "asterfort/gilig2.h"
#include "asterfort/gilig3.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: nfic, niv, ndim, nbobo
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
!
!     BUT: LIRE LE FICHIER DE MAILLAGE GIBI (PROCEDURE SAUVER) :
!
!     IN : NFIC  : UNITE DE LECTURE
!          NIV   : NUMERO DU NIVEAU GIBI
!     OUT: NDIM  : DIMENSION DU PROBLEME (2D OU 3D)
!          NBOBO : NOMBRE D'OBJETS (AU SENS GIBI)
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: r8bid
    integer :: nbobno, ipile, nivo, nberr, nboblu
    character(len=1) :: ityp
    character(len=4) :: k4bid, kbid4
    character(len=6) :: k6bid
    character(len=14) :: kbid14
    logical :: legrno
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iaptin, iret, nbnoto, nbval
!-----------------------------------------------------------------------
    call jemarq()
!
    legrno = .false.
 1  continue
    read(nfic,1001,end=9997) kbid14, kbid4, ityp
!
    if (kbid14 .eq. 'ENREGISTREMENT' .and. kbid4 .eq. 'TYPE') then
!
        if (ityp .eq. '4') then
!
! -- INFORMATIONS GENERALES MAILLAGE ----
!
            read(nfic,1002) nivo, nberr, ndim
            if (nberr .gt. 0) then
                call u2mess('A', 'PREPOST_59')
            endif
            read(nfic,1003) r8bid
            goto 1
!
        else if (ityp .eq. '7') then
!
! -- INFORMATIONS GENERALES CASTEM 2000 ----
!
            read(nfic,1004)
            read(nfic,1004)
            goto 1
!
        else if (ityp .eq. '5') then
!
!          -- ON A TOUT LU ----
!
            goto 9997
!
        else if (ityp .eq. '2') then
!
! -- LECTURE D'UNE PILE  ----
!
            if (niv .le. 6) then
                read(nfic,1005) k4bid,k6bid,ipile,nbobno,nboblu
            else if (niv .gt. 6) then
                read(nfic,1006) k4bid,k6bid,ipile,nbobno,nboblu
            endif
!
!
            if (ipile .eq. 0) then
!
!            --- LECTURE DES GROUPES DE NOEUDS NOMMES ---
                legrno = .true.
                call gilig2(nfic, nbobno, niv)
!
!            --- LECTURE DES COORDONNEES ---
                nbval = nboblu * ( ndim + 1 )
                call gilig1(nfic, ndim, nbval, nboblu)
!
                nbnoto = nboblu
!
                call jeexin('&&GILIRE.INDIRECT', iret)
                if (iret .eq. 0) then
                    call wkvect('&&GILIRE.INDIRECT', 'V V I', nboblu, iaptin)
                    do 10 i = 1, nboblu
                        zi(iaptin+i-1) = i
10                  continue
                endif
                goto 1
!
            else if (ipile .eq. 32) then
!
!            --- LECTURE DES GROUPES DE NOEUDS NOMMES ---
                if (legrno) goto 1
                call gilig3(nfic, nbobno, niv, nboblu)
                goto 1
!
            else if (ipile .eq. 33) then
!
                read(nfic,1010) nbval
!
!            --- LECTURE DES COORDONNEES ---
                nboblu = nbval / ( ndim + 1 )
                call gilig1(nfic, ndim, nbval, nboblu)
                nbnoto = nboblu
!
                goto 1
!
            else if (ipile .eq. 1) then
!
!            --- LECTURE DES GROUPES DE MAILLES NOMMEES ---
                call gilig0(nfic, nboblu, nbobno, nbobo, niv)
                goto 1
            endif
!
        endif
        goto 1
!
    else
        goto 1
    endif
!
9997  continue
!
!     -- ON CREE .CONNEX2:
    call gicnx2()
!
!     -- ON CREE .NUMANEW:
    call gidoma(nbnoto)
!
!
!
    1001 format(1x,a14,4x,a4,3x,a1)
    1002 format(7x,i4,14x,i4,10x,i4)
    1003 format(8x,d12.5)
    1004 format(10x)
    1005 format(1x,a4,1x,a6,i4,18x,i5,11x,i5)
    1006 format(1x,a4,1x,a6,i4,18x,i8,11x,i8)
    1010 format(1x,i7)
!
    call jedema()
end subroutine
