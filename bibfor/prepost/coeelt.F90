subroutine coeelt(imod, nbtyma, nomail, nbnoma, nuconn,&
                  nbmail)
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
!
#include "asterfort/codent.h"
#include "asterfort/codnop.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: imod, nbtyma, nbmail, nbnoma(15), nuconn(15, 32)
    character(len=8) :: nomail(*)
! -----  VARIABLES LOCALES
    integer :: neu2(32), ier
    integer :: i, jnuma, jnbnma, jnoma, jnbmag, jnbtym, jindma
    integer :: nte, ij, ima, ityp, nbno, inum, nbnoas, ino
    integer :: idiv, irest, mod, k, l, jtypma, indmax, maxmai
    integer :: max, jgrmai, numgro, vali(2), jgr
    character(len=1) :: prfnoe, prfmai
    character(len=8) :: chgrou, chtab(32), chmail, k8bid
    character(len=12) :: chenti
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
    do 10 i = 1, 32
        chtab(i) = '        '
10  end do
!
! --- RECUPERATION DES OBJETS DE TRAVAIL :
!     ----------------------------------
    call jeveuo('&&PRECOU.NUMERO.MAILLES', 'L', jnuma)
    call jeveuo('&&PRECOU.TYPE.MAILLES', 'L', jtypma)
    call jeveuo('&&PRECOU.NBNO.MAILLES', 'L', jnbnma)
    call jeveuo('&&PRECOU.CONNEC.MAILLES', 'L', jnoma)
    call jeveuo('&&PRECOU.NBMA.GROUP_MA', 'L', jnbmag)
    call jeveuo('&&PRECOU.NBTYP.MAILLES', 'L', jnbtym)
    call jeveuo('&&PRECOU.INDICE.GROUP_MA', 'L', jindma)
!
! --- ECRITURE DES MAILLES :
!     --------------------
    do 20 nte = 1, nbtyma
!
        if (zi(jnbtym+nte-1) .eq. 0) goto 20
        call codent(zi(jnbtym+nte-1), 'G', chenti(7:12))
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
        do 30 ima = 1, nbmail
            ityp = zi(jtypma+ima-1)
            nbno = zi(jnbnma+ima-1)
            if (ityp .eq. nte) then
                inum = zi(jnuma+ima-1)
                call codnop(chmail, prfmai, 1, 1)
                call codent(inum, 'G', chmail(2:8))
!
                nbnoas = nbnoma(nte)
!
                do 40 ino = 1, nbnoas
                    neu2(ino) = zi(jnoma+ij+nuconn(nte,ino)-1)
                    call codnop(chtab(ino), prfnoe, 1, 1)
                    call codent(neu2(ino), 'G', chtab(ino)(2:8))
40              continue
!
                idiv = int(nbnoas/8)
                irest = mod(nbnoas,8)
!
                if (irest .ne. 0) then
                    write(imod,202) chmail,(chtab(i),i=1,nbnoas)
                else
                    do 1000 k = 1, idiv
                        l = 8*(k-1)
                        if (idiv .eq. 1) then
                            write(imod,'(A,8(1X,A))') chmail,(chtab(i)&
                            ,i=1+l,8+l)
                        else
                            write(imod,'(8X,8(1X,A))') (chtab(i),i=1+&
                            l,8+l)
                        endif
1000                  continue
                endif
            endif
            ij = ij + nbno
30      continue
!
        write(imod,'(A)') 'FINSF'
        write(imod,'(A)') '%'
!
20  end do
!
    202 format(a,8(1x,a),/,(8x,8(1x,a)))
!
! --- ECRITURE DES GROUP_MA :
!     ---------------------
    call jelira('&&PRECOU.INDICE.GROUP_MA', 'LONUTI', indmax)
!
    maxmai = 0
    do 50 i = 1, indmax
        maxmai = max(maxmai,zi(jnbmag+i-1))
50  end do
!
    call wkvect('&&PRECOU.GRMA.MAILLES', 'V V K24', maxmai, jgrmai)
!
    chgrou(1:2) = 'GM'
!
! --- BOUCLE SUR LES GROUPES DE MAILLES :
!     ---------------------------------
    ier = 0
    do 60 i = 1, indmax
        numgro = zi(jindma+i-1)
!MH
!   NUMGRO = 1
!MH
        if (numgro .ge. 1000000) then
            ier = ier + 1
            vali(1) = numgro
            vali(2) = 1000000
            call u2mesg('E', 'PREPOST5_21', 0, ' ', 0,&
                        vali, 0, 0.d0)
            goto 60
        endif
        call codent(numgro, 'G', chgrou(3:8))
        write(imod,'(A,4X,2A)') 'GROUP_MA','NOM=',chgrou
        call jeveuo(jexnum('&&PRECOU.LISTE.GROUP_MA', i), 'E', jgr)
        do 70 k = 1, zi(jnbmag+i-1)
            call codnop(chmail, prfmai, 1, 1)
            call codent(zi(jgr+k-1), 'G', chmail(2:8))
            zk24(jgrmai+k-1) = chmail
70      continue
!
! ---   ECRITURE DES MAILLES DU GROUPE DE MAILLES COURANT :
!       -------------------------------------------------
        write(imod,'(24(2X,A))') (zk24(jgrmai+k-1),k=1,zi(jnbmag+i-1))
!
        write(imod,'(A)') 'FINSF'
        write(imod,'(A)') '%'
!
!
60  end do
!
!
    if (ier .ne. 0) then
        call u2mess('F', 'PREPOST_60')
    endif
!
    call jedema()
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine
