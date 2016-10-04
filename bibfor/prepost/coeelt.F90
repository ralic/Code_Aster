subroutine coeelt(imod, nbtyma, nomail, nbnoma, nuconn,&
                  nbmail)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.greffet at edf.fr
    implicit none
!
!      COEELT --   ECRITURE DES MAILLES ET DES GROUP_MA VENANT
!                  D'UN FICHIER .GMSH DANS LE FICHIER .MAIL
!
!   ARGUMENT        E/S  TYPE         ROLE
!    NBTYMA         IN    I         NOMBRE  DE TYPES DE MAILLES
!    NOMAIL(*)      IN    K8        TABLEAU DES NOMS DES TYPES DE MAILLE
!    NBMAIL         IN    I         NOMBRE TOTAL DE MAILLES
!    NUCONN         IN    I         PASSAGE DE LA NUMEROTATION DES NDS
!                                     D'UNE MAILLE : ASTER -> GMSH
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/codnop.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: imod, nbtyma, nbmail, nbnoma(15), nuconn(15, 32)
    character(len=8) :: nomail(*)
! -----  VARIABLES LOCALES
    integer :: neu2(32), ier
    integer :: i
    integer :: nte, ij, ima, ityp, nbno, inum, nbnoas, ino
    integer :: idiv, irest, mod, k, l,  indmax, maxmai
    integer :: max, jgrmai, numgro, jgr
    character(len=1) :: prfnoe, prfmai
    character(len=8) :: chgrou, chtab(32), chmail, k8bid
    character(len=12) :: chenti
    integer, pointer :: indma(:) => null()
    integer, pointer :: nbmag(:) => null()
    integer, pointer :: nbnma(:) => null()
    integer, pointer :: nbtym(:) => null()
    integer, pointer :: noma(:) => null()
    integer, pointer :: numa(:) => null()
    integer, pointer :: typma(:) => null()
!
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
    call jemarq()
!
! --- INITIALISATIONS :
!     ---------------
    prfmai = 'M'
    prfnoe = 'N'
    chgrou = '        '
    chmail = '        '
    k8bid = '        '
    chenti = 'NBOBJ=      '
!
    do i = 1, 32
        chtab(i) = '        '
    end do
!
! --- RECUPERATION DES OBJETS DE TRAVAIL :
!     ----------------------------------
    call jeveuo('&&PRECOU.NUMERO.MAILLES', 'L', vi=numa)
    call jeveuo('&&PRECOU.TYPE.MAILLES', 'L', vi=typma)
    call jeveuo('&&PRECOU.NBNO.MAILLES', 'L', vi=nbnma)
    call jeveuo('&&PRECOU.CONNEC.MAILLES', 'L', vi=noma)
    call jeveuo('&&PRECOU.NBMA.GROUP_MA', 'L', vi=nbmag)
    call jeveuo('&&PRECOU.NBTYP.MAILLES', 'L', vi=nbtym)
    call jeveuo('&&PRECOU.INDICE.GROUP_MA', 'L', vi=indma)
!
! --- ECRITURE DES MAILLES :
!     --------------------
    do nte = 1, nbtyma
!
        if (nbtym(nte) .eq. 0) cycle
        call codent(nbtym(nte), 'G', chenti(7:12))
!
! ---   ECRITURE DE LA DATE :
!       -------------------
        write(unit=imod,fmt='(A,3X,A,3X,A)') nomail(nte),&
     &    'NOM=INDEFINI',chenti
!
        ij = 0
!
! ---   BOUCLE SUR LES MAILLES :
!       ----------------------
        do ima = 1, nbmail
            ityp = typma(ima)
            nbno = nbnma(ima)
            if (ityp .eq. nte) then
                inum = numa(ima)
                call codnop(chmail, prfmai, 1, 1)
                call codent(inum, 'G', chmail(2:8))
!
                nbnoas = nbnoma(nte)
!
                do ino = 1, nbnoas
                    neu2(ino) = noma(1+ij+nuconn(nte,ino)-1)
                    call codnop(chtab(ino), prfnoe, 1, 1)
                    call codent(neu2(ino), 'G', chtab(ino)(2:8))
                end do
!
                idiv = int(nbnoas/8)
                irest = mod(nbnoas,8)
!
                if (irest .ne. 0) then
                    write(imod,202) chmail,(chtab(i),i=1,nbnoas)
                else
                    do k = 1, idiv
                        l = 8*(k-1)
                        if (idiv .eq. 1) then
                            write(imod,'(8(1x,a8))') chmail,(chtab(i)&
                            ,i=1+l,8+l)
                        else
                            write(imod,'(9x,7(1x,a8))') (chtab(i),i=1+&
                            l,8+l)
                        endif
                    end do
                endif
            endif
            ij = ij + nbno
        end do
!
        write(imod,'(A)') 'FINSF'
        write(imod,'(A)') '%'
!
    end do
!
    202 format(a,8(1x,a8),/,(9x,8(1x,a8)))
!
! --- ECRITURE DES GROUP_MA :
!     ---------------------
    call jelira('&&PRECOU.INDICE.GROUP_MA', 'LONUTI', indmax)
!
    maxmai = 0
    do i = 1, indmax
        maxmai = max(maxmai,nbmag(i))
    end do
!
    call wkvect('&&PRECOU.GRMA.MAILLES', 'V V K24', maxmai, jgrmai)
!
    chgrou(1:2) = 'GM'
!
! --- BOUCLE SUR LES GROUPES DE MAILLES :
!     ---------------------------------
    ier = 0
    do i = 1, indmax
        numgro = indma(i)
        if (numgro .ge. 1000000) then
            ier = ier + 1
            call utmess('E', 'PREPOST5_21', ni=2, vali=[numgro, 1000000])
            cycle
        endif
        call codent(numgro, 'G', chgrou(3:8))
        write(imod,'(A,4X,2A)') 'GROUP_MA','NOM=',chgrou
        call jeveuo(jexnum('&&PRECOU.LISTE.GROUP_MA', i), 'E', jgr)
        do k = 1, nbmag(i)
            call codnop(chmail, prfmai, 1, 1)
            call codent(zi(jgr+k-1), 'G', chmail(2:8))
            zk24(jgrmai+k-1) = chmail
        end do
!
! ---   ECRITURE DES MAILLES DU GROUPE DE MAILLES COURANT :
!       -------------------------------------------------
        write(imod,'(7(1X,A8))') (zk24(jgrmai+k-1),k=1,nbmag(i))
!
        write(imod,'(A)') 'FINSF'
        write(imod,'(A)') '%'
!
!
    end do
!
!
    if (ier .ne. 0) then
        call utmess('F', 'PREPOST_60')
    endif
!
    call jedema()
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine
