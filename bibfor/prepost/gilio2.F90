subroutine gilio2(nfic, iobj, nbele, niv)
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
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: nfic, iobj, nbele, niv
! ----------------------------------------------------------------------
!     BUT: LIRE LES N LIGNES REPRESENTANT UN OBJET (AU SENS GIBI).
!                  (PROCEDURE SAUVER)
!
!     IN: NFIC : UNITE DE LECTURE.
!         IOBJ : NUMERO DE L'OBJET QUE L'ON VA LIRE.
!         NIV  : NIVEAU GIBI
!     OUT:NBELE: NOMBRE DE MAILLES CONTENUES DANS L'OBJET.
!
! ----------------------------------------------------------------------
!
!     VARIABLES LOCALES:
    integer :: itypel, iret
    character(len=8) :: typmai
    character(len=5) :: k5bid
    character(len=16) :: k16obj
    integer :: nbnum, vali(5)
!
!
!-----------------------------------------------------------------------
    integer :: i, iacnex, iacoul, iadsob, ianoob, iarefe, iasoob
    integer :: icoj, j, nbfois, nbno, nbref, nbrest, nbsoob
!
!-----------------------------------------------------------------------
    call jemarq()
    if (niv .eq. 3) then
        nbnum = 16
        read (nfic,1008) itypel,nbsoob,nbref,nbno,nbele
    else
        nbnum = 10
        read (nfic,1009) itypel,nbsoob,nbref,nbno,nbele
    endif
!
    if ((itypel.eq.0) .and. (nbsoob.eq.0)) then
        call utmess('F', 'PREPOST4_95')
    endif
! ---------------------------------
    call jeexin('&&GILIRE.NOMOBJ', iret)
    if (iret .eq. 0) then
        call utmess('F', 'PREPOST_46')
    endif
    call jeveuo('&&GILIRE.NOMOBJ', 'E', ianoob)
    call jeveuo('&&GILIRE.DESCOBJ', 'E', iadsob)
    call codent(iobj, 'D', k5bid)
    zk8(ianoob-1+2* (iobj-1)+1) = '.OB'//k5bid
!
! -- CONVERSION NUMERO TYPE MAILLE AVEC CODE TYPE MAILLE --
!
    typmai = 'INCONNU'
!
    if (itypel .eq. 1) then
        typmai = 'POI1'
!
    else if (itypel.eq.2) then
        typmai = 'SEG2'
!
    else if (itypel.eq.3) then
        typmai = 'SEG3'
!
    else if (itypel.eq.4) then
        typmai = 'TRI3'
!
    else if (itypel.eq.6) then
        typmai = 'TRI6'
!
    else if (itypel.eq.8) then
        typmai = 'QUA4'
!
    else if (itypel.eq.10) then
        typmai = 'QUA8'
!
    else if (itypel.eq.11) then
        typmai = 'QUA9'
!
    else if (itypel.eq.14) then
        typmai = 'CUB8'
!
    else if (itypel.eq.15) then
        typmai = 'CU20'
!
    else if (itypel.eq.33) then
        typmai = 'CU27'
!
    else if (itypel.eq.16) then
        typmai = 'PRI6'
!
    else if (itypel.eq.17) then
        typmai = 'PR15'
!
    else if (itypel.eq.23) then
        typmai = 'TET4'
!
    else if (itypel.eq.24) then
        typmai = 'TE10'
!
    else if (itypel.eq.25) then
        typmai = 'PYR5'
!
    else if (itypel.eq.26) then
        typmai = 'PY13'
!
! --- SI ITYPEL = 0, ON S'EST ASSURE QUE NBSOOB > 0
    else if (itypel.ne.0) then
        vali(1) = itypel
        vali(2) = nbsoob
        vali(3) = nbref
        vali(4) = nbno
        vali(5) = nbele
        call utmess('F', 'PREPOST4_94', ni=5, vali=vali)
    endif
!
    zk8(ianoob-1+2* (iobj-1)+2) = typmai
    zi(iadsob-1+4* (iobj-1)+1) = nbsoob
    zi(iadsob-1+4* (iobj-1)+2) = nbref
    zi(iadsob-1+4* (iobj-1)+3) = nbno
    zi(iadsob-1+4* (iobj-1)+4) = nbele
!
! ---------------------------------
    k16obj = '&&GILIRE'//'.OB'//k5bid
    if (nbsoob .gt. 0) then
!
!         -- CAS OBJET DECOMPOSE:
!         -----------------------
        call wkvect(k16obj//'.SOUSOB', 'V V I', nbsoob, iasoob)
        nbfois = nbsoob/nbnum
        nbrest = nbsoob - nbnum*nbfois
        icoj = 0
!
!        -- ON LIT LES NUMEROS DES SOUS-OBJETS:
!        --------------------------------------
        do 1,i = 1,nbfois
        if (niv .eq. 3) then
            read(nfic,1011) (zi(iasoob-1+j),j=icoj+1,icoj+nbnum)
        else
            read(nfic,1010) (zi(iasoob-1+j),j=icoj+1,icoj+nbnum)
        endif
        icoj = icoj + nbnum
 1      continue
        if (nbrest .gt. 0) then
            if (niv .eq. 3) then
                read(nfic,1011) (zi(iasoob-1+j),j=icoj+1,icoj+nbrest)
            else
                read(nfic,1010) (zi(iasoob-1+j),j=icoj+1,icoj+nbrest)
            endif
        endif
!
!        -- ON LIT LES REFERENCES:
!        -------------------------
        if (nbref .gt. 0) then
            call wkvect(k16obj//'.REFE  ', 'V V I', nbref, iarefe)
            nbfois = nbref/nbnum
            nbrest = nbref - nbnum*nbfois
            icoj = 0
            do 2,i = 1,nbfois
            if (niv .eq. 3) then
                read (nfic,1011) (zi(iarefe-1+j),j=icoj+1,icoj+&
                    nbnum)
            else
                read (nfic,1010) (zi(iarefe-1+j),j=icoj+1,icoj+&
                    nbnum)
            endif
            icoj = icoj + nbnum
 2          continue
            if (nbrest .gt. 0) then
                if (niv .eq. 3) then
                    read (nfic,1011) (zi(iarefe-1+j),j=icoj+1,icoj+&
                    nbrest)
                else
                    read (nfic,1010) (zi(iarefe-1+j),j=icoj+1,icoj+&
                    nbrest)
                endif
            endif
        endif
!
    else
!
!        -- CAS "OBJET SIMPLE":
!        ----------------------
!        -- ON LIT LES REFERENCES:
!        -------------------------
        if (nbref .gt. 0) then
            call wkvect(k16obj//'.REFE  ', 'V V I', nbref, iarefe)
            nbfois = nbref/nbnum
            nbrest = nbref - nbnum*nbfois
            icoj = 0
            do 3,i = 1,nbfois
            if (niv .eq. 3) then
                read(nfic,1011) (zi(iarefe-1+j),j=icoj+1,icoj+&
                    nbnum)
            else
                read(nfic,1010) (zi(iarefe-1+j),j=icoj+1,icoj+&
                    nbnum)
            endif
            icoj = icoj + nbnum
 3          continue
            if (nbrest .gt. 0) then
                if (niv .eq. 3) then
                    read (nfic,1011) (zi(iarefe-1+j),j=icoj+1,icoj+&
                    nbrest)
                else
                    read (nfic,1010) (zi(iarefe-1+j),j=icoj+1,icoj+&
                    nbrest)
                endif
            endif
        endif
!
!        -- ON LIT LES COULEURS DES ELEMENTS:
!        ------------------------------------
        if (nbele .gt. 0) then
            call wkvect(k16obj//'.COULEU', 'V V I ', nbele, iacoul)
            nbfois = nbele/nbnum
            nbrest = nbele - nbnum*nbfois
            icoj = 0
            do 4,i = 1,nbfois
            if (niv .eq. 3) then
                read (nfic,1011) (zi(iacoul-1+j),j=icoj+1,icoj+&
                    nbnum)
            else
                read (nfic,1010) (zi(iacoul-1+j),j=icoj+1,icoj+&
                    nbnum)
            endif
            icoj = icoj + nbnum
 4          continue
            if (nbrest .gt. 0) then
                if (niv .eq. 3) then
                    read (nfic,1011) (zi(iacoul-1+j),j=icoj+1,icoj+&
                    nbrest)
                else
                    read (nfic,1010) (zi(iacoul-1+j),j=icoj+1,icoj+&
                    nbrest)
                endif
            endif
!
!          -- ON LIT LA CONNECTIVITE DES ELEMENTS:
!          ----------------------------------------
            call wkvect(k16obj//'.CONNEX', 'V V I', nbele*nbno, iacnex)
            nbfois = nbele*nbno/nbnum
            nbrest = nbele*nbno - nbnum*nbfois
            icoj = 0
            do 5,i = 1,nbfois
            if (niv .eq. 3) then
                read (nfic,1011) (zi(iacnex-1+j),j=icoj+1,icoj+&
                    nbnum)
            else
                read (nfic,1010) (zi(iacnex-1+j),j=icoj+1,icoj+&
                    nbnum)
            endif
            icoj = icoj + nbnum
 5          continue
            if (nbrest .gt. 0) then
                if (niv .eq. 3) then
                    read (nfic,1011) (zi(iacnex-1+j),j=icoj+1,icoj+&
                    nbrest)
                else
                    read (nfic,1010) (zi(iacnex-1+j),j=icoj+1,icoj+&
                    nbrest)
                endif
            endif
        endif
!
    endif
!
!
    1008 format (i5,i5,i5,i5,i5)
    1009 format (i8,i8,i8,i8,i8)
    1010 format (10 (i8))
    1011 format (16 (i5))
!
    call jedema()
end subroutine
