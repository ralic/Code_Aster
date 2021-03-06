subroutine gmeelt(imod, nbtyma, nomail, nbnoma, nuconn,&
                  nbmail)
    implicit none
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
    integer :: imod, nbtyma, nbmail, nbnoma(19), nuconn(19, 32)
    character(len=8) :: nomail(*)
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
!
!      GMEELT --   ECRITURE DES MAILLES ET DES GROUP_MA VENANT
!                  D'UN FICHIER .GMSH DANS LE FICHIER .MAIL
!
!   ARGUMENT        E/S  TYPE         ROLE
!    NBTYMA         IN    I         NOMBRE  DE TYPES DE MAILLES
!    NOMAIL(*)      IN    K8        TABLEAU DES NOMS DES TYPES DE MAILLE
!    NBMAIL         IN    I         NOMBRE TOTAL DE MAILLES
!    NUCONN         IN    I         PASSAGE DE LA NUMEROTATION DES NDS
!                                     D'UNE MAILLE : ASTER -> GMSH
!
!
!
!
    integer :: neu2(32), ier, i, ij, nte, ima, ityp, nbno, inum, nbnoas
    integer :: idiv, ino, irest, k, l, maxmai, numgro, jgrmai, jgr, ima1
    integer :: indmax, vali(2)
    character(len=1) :: prfnoe, prfmai
    character(len=8) :: chgrou, chtab(32), chmail, k8bid
    character(len=12) :: chenti
    integer, pointer :: nbnma(:) => null()
    integer, pointer :: nbtym(:) => null()
    integer, pointer :: noma(:) => null()
    integer, pointer :: numa(:) => null()
    integer, pointer :: typma(:) => null()
    integer, pointer :: indma(:) => null()
    integer, pointer :: nbmag(:) => null()
!
! ----------------------------------------------------------------------
!
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
    call jeveuo('&&PREGMS.NUMERO.MAILLES', 'L', vi=numa)
    call jeveuo('&&PREGMS.TYPE.MAILLES', 'L', vi=typma)
    call jeveuo('&&PREGMS.NBNO.MAILLES', 'L', vi=nbnma)
    call jeveuo('&&PREGMS.CONNEC.MAILLES', 'L', vi=noma)
    call jeveuo('&&PREGMS.NBMA.GROUP_MA', 'L', vi=nbmag)
    call jeveuo('&&PREGMS.NBTYP.MAILLES', 'L', vi=nbtym)
    call jeveuo('&&PREGMS.INDICE.GROUP_MA', 'L', vi=indma)
!
! --- ECRITURE DES MAILLES :
!     --------------------
    do 20 nte = 1, nbtyma
!
        if (nbtym(nte) .eq. 0) goto 20
        call codent(nbtym(nte), 'G', chenti(7:12))
!
! ---   ECRITURE DE LA DATE :
!       -------------------
        write(unit=imod,fmt='(A,3X,A,3X,A)') nomail(nte),&
     &    'NOM=INDEFINI',chenti
!
        ij = 0
!
! --- ON VERIFIE QUE LE NOMBRE MAX DE MAILLES N'EST PAS ATTEINT
!     LA LIMITE EST DE 9 999 999 MAILLES
!
        if (nbmail .ge. 10000000) then
            vali (1) = nbmail
            call utmess('E', 'PREPOST6_43', si=vali(1))
        endif
!
! ---   BOUCLE SUR LES MAILLES :
!       ----------------------
        do 30 ima = 1, nbmail
            ityp = typma(ima)
            nbno = nbnma(ima)
            if (ityp .eq. nte) then
                inum = numa(ima)
                call codnop(chmail, prfmai, 1, 1)
                call codent(inum, 'G', chmail(2:8))
!
                nbnoas = nbnoma(nte)
!
                do 40 ino = 1, nbnoas
                    neu2(ino) = noma(1+ij+nuconn(nte,ino)-1)
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
    ier = 0
    call jelira('&&PREGMS.INDICE.GROUP_MA', 'LONUTI', indmax)
!
    maxmai = 0
    do 50 i = 1, indmax
        maxmai = max(maxmai,nbmag(i))
50  end do
!
! --- SI IL N Y A AU MOINS UN GROUPE :
!     ------------------------------
    if (maxmai .ne. 0) then
!
        call wkvect('&&PREGMS.GRMA.MAILLES', 'V V K8', maxmai, jgrmai)
!
        chgrou(1:2) = 'GM'
!
! --- BOUCLE SUR LES GROUPES DE MAILLES :
!     ---------------------------------
        do 60 i = 1, indmax
            numgro = indma(i)
            if (numgro .ge. 1000000) then
                ier = ier + 1
                vali (1) = numgro
                vali (2) = 1000000
                call utmess('E', 'PREPOST5_21', ni=2, vali=vali)
                goto 60
            endif
            call codent(numgro, 'G', chgrou(3:8))
            write(imod,'(A,4X,2A)') 'GROUP_MA','NOM=',chgrou
            call jeveuo(jexnum('&&PREGMS.LISTE.GROUP_MA', i), 'E', jgr)
            do 70 k = 1, nbmag(i)
                call codnop(chmail, prfmai, 1, 1)
                call codent(zi(jgr+k-1), 'G', chmail(2:8))
                zk8(jgrmai+k-1) = chmail
70          continue
!
! ---   ECRITURE DES MAILLES DU GROUPE DE MAILLES COURANT :
!       -------------------------------------------------
            write(imod,'(8(2X,A))') (zk8(jgrmai+k-1),k=1,nbmag(1+i-&
            1))
!
            write(imod,'(A)') 'FINSF'
            write(imod,'(A)') '%'
!
! --- DANS LE CAS D'UN POINT ECRITURE D'UN GROUPNO
! ---  LE GROUPE DE MAILLE CONTIENT ALORS UNE SEULE MAILLE POI1
!
            if (nbmag(i) .eq. 1) then
                call jeveuo(jexnum('&&PREGMS.LISTE.GROUP_MA', i), 'E', jgr)
                ima1=zi(jgr)
                ij=0
                do 80 ima = 1, nbmail
                    inum = numa(ima)
                    nbno = nbnma(ima)
                    if (inum .eq. ima1) then
                        if (nbno .eq. 1) then
                            write(imod,'(A,4X,2A)') 'GROUP_NO','NOM=',&
                            chgrou
                            neu2(ino) = noma(ij+1)
                            call codnop(chtab(ino), prfnoe, 1, 1)
                            call codent(neu2(ino), 'G', chtab(ino)(2:8))
                            write(imod,'((2X,A))') chtab(ino)
                            write(imod,'(A)') 'FINSF'
                            write(imod,'(A)') '%'
                            goto 90
                        endif
                    endif
                    ij = ij + nbno
80              continue
            endif
90          continue
!
60      continue
!
    endif
!
    if (ier .ne. 0) then
        call utmess('F', 'PREPOST_60')
    endif
!
    call jedema()
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine
