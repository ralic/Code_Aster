subroutine creaco(nbmato, ma, nblien)
!
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
!    - FONCTION REALISEE:
!       - CREATION DE LA CONNECTIVITE DES MAILLES
!
!    - IN :     NBMATO : NOMBRE DE MAILLES
!               RENUM  : RENUMEROTATION
!               MA     : NOM DU MAILLAGE
!               NUMSDM : SOUS DOMAINES DE CHAQUES MAILLE
!
!    - OUT :    RENUM2 : RENUMEROTATION
!               RENUM2 : RENUMEROTATION INVERSE DE RENUM2
!               CO     : CONNECTIVITE DES MAILLES
!               IDCO   : INDEX DE CO
!               NBLIEN : NOMBRE DE LIEN
!               NBMAMA : NOMBRE DE MAILLES RELIEES A CHAQUE MAILLE
!
!----------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
!
    implicit none
!
!
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
!
    integer :: nbmato, renum, renum2, renum3, co, idco, nbmama
    integer :: nblien
    character(len=8) :: ma
    integer :: nbmano, idcoi, id1, typma, idno, nbno, idnoeu, nbnoeu, ima, i, j
    integer :: mail, ino, id, nbre, id2, temp, temp1, maxi, ifm, niv, coi
    integer :: numno,  nbnoto
    real(kind=8) :: tmps(7)
    character(len=8) :: nom, typma1, typma2
    integer, pointer :: typmail(:) => null()
!
!----------------------------------------------------------------------
    call jemarq()
    call infniv(ifm, niv)
!
    if (niv .ge. 2) then
        call uttcpu('CPU.CREACO', 'INIT', ' ')
        call uttcpu('CPU.CREACO', 'DEBUT', ' ')
    endif
!
    call jeveuo('&&FETSKP.RENUM', 'L', renum)
!
    call dismoi('NB_NO_MAILLA', ma, 'MAILLAGE', repi=nbnoto)
!
    write(ifm,*)' -- NOMBRE DE MAILLES : ',nbmato
    write(ifm,*)' -- NOMBRE DE NOEUDS  : ',nbnoto
    write(ifm,*)' '
!
    call wkvect('&&FETSKP.NBMANO', 'V V I', nbnoto, nbmano)
    call wkvect('&&FETSKP.IDCOI', 'V V I', nbnoto+1, idcoi)
    call wkvect('&&FETSKP.ID1', 'V V I', nbnoto, id1)
    call wkvect('&&FETSKP.TYPMA', 'V V K8', nbmato, typma)
    call wkvect('&&FETSKP.IDNO', 'V V I', nbmato, idno)
    call wkvect('&&FETSKP.NBNO', 'V V I', nbmato, nbno)
!
! ------- ON RECUPERE LE NOMBRE DE MAILLES RELIEES A CHAQUE NOEUD ----
! ------- CREATION DU TABLEAU DES TYPES DE MAILLES -------------------
! ------- REMPLISSAGE DES TABLEAUX NBNO ET IDNOEU --------------------
! ------- LINEARISATION DES ELEMENTS ---------------------------------
!
    call jeveuo(ma//'.TYPMAIL', 'L', vi=typmail)
!
    do ima = 1, nbmato
        mail=zi(renum-1+ima)
        nbre=typmail(mail)
        call jenuno(jexnum('&CATA.TM.NOMTM', nbre), nom)
        call jeveuo(jexnum(ma//'.CONNEX', mail), 'L', idnoeu)
        call jelira(jexnum(ma//'.CONNEX', mail), 'LONMAX', nbnoeu)
        zi(idno-1+ima)=idnoeu
!
!      ------- ON LINEARISE LES ELEMENTS -------
        if (nom .eq. 'SEG3    ') then
            zk8(typma-1+ima)= 'SEG2    '
            zi(nbno-1+ima)=2
        else if (nom .eq. 'TRIA6   ') then
            zk8(typma-1+ima)= 'TRIA3   '
            zi(nbno-1+ima)=3
        else if (nom .eq. 'QUAD8   ') then
            zk8(typma-1+ima)= 'QUAD4   '
            zi(nbno-1+ima)=4
        else if (nom .eq. 'QUAD9   ') then
            zk8(typma-1+ima)= 'QUAD4   '
            zi(nbno-1+ima)=4
        else if (nom .eq. 'TETRA10 ') then
            zk8(typma-1+ima)= 'TETRA4  '
            zi(nbno-1+ima)=4
        else if (nom .eq. 'PENTA15 ') then
            zk8(typma-1+ima)= 'PENTA6  '
            zi(nbno-1+ima)=6
        else if (nom .eq. 'PENTA18 ') then
            zk8(typma-1+ima)= 'PENTA6  '
            zi(nbno-1+ima)=6
        else if (nom .eq. 'HEXA20  ') then
            zk8(typma-1+ima)= 'HEXA8   '
            zi(nbno-1+ima)=8
        else if (nom .eq. 'HEXA27  ') then
            zk8(typma-1+ima)= 'HEXA8   '
            zi(nbno-1+ima)=8
        else if (nom .eq. 'PYRAM13 ') then
            zk8(typma-1+ima)= 'PYRAM5  '
            zi(nbno-1+ima)=5
        else
            zk8(typma-1+ima)= nom
            zi(nbno-1+ima)=nbnoeu
        endif
!      ------- FIN DE LA LINEARISATION -------
!
        do ino = 1, zi(nbno-1+ima)
            numno = zi(idnoeu-1+ino)
            zi(nbmano-1+numno)=zi(nbmano-1+numno)+1
        end do
    end do
!
! ------- ON CREE LE TABLEAU D'INDEX POUR COI ------------------------
!
    zi(idcoi)=1
    do ino = 2, nbnoto+1
        zi(idcoi-1+ino)=zi(idcoi-1+ino-1)+zi(nbmano-1+ino-1)
    end do
!
! ------- ON CREE LE TABLEAU DE CONNECTIVITE INVERSE ( COI ) ---------
!
    call wkvect('&&FETSKP.COI', 'V V I', zi(idcoi-1+nbnoto+1)-1, coi)
!
    do ima = 1, nbmato
        idnoeu=zi(idno-1+ima)
        do ino = 1, zi(nbno-1+ima)
            numno=zi(idnoeu-1+ino)
            id=zi(idcoi-1+numno)+zi(id1-1+numno)
            zi(coi-1+id)=ima
            zi(id1-1+numno)=zi(id1-1+numno)+1
        end do
    end do
!
    call jedetr('&&FETSKP.ID1')
!
! ------- ON REMPLIT LE TABLEAU NOMBRE DE MAILLES PAR MAILLE (NBMAMA)
!
    call wkvect('&&FETSKP.NBMAMA', 'V V I', nbmato, nbmama)
!
    do ino = 1, nbnoto
        if (zi(nbmano-1+ino) .gt. 1) then
            do i = zi(idcoi-1+ino), zi(idcoi-1+ino+1)-1
                nbre=zi(idcoi-1+ino+1)-1-zi(idcoi-1+ino)
                mail=zi(coi-1+i)
                zi(nbmama-1+mail)=zi(nbmama-1+mail)+nbre
            end do
        endif
    end do
!
    call jedetr('&&FETSKP.NBMANO')
!
! ------- ON REMPLIT L'INDEX DU TABLEAU DES CONNECTIVITES (CO) -------
!
    call wkvect('&&FETSKP.IDCO', 'V V S', nbmato+1, idco)
!
    maxi=0
    zi4(idco)=1
    do ima = 2, nbmato+1
        if (zi(nbmama-1+ima-1) .gt. maxi) maxi=zi(nbmama-1+ima-1)
        zi4(idco-1+ima)=zi4(idco-1+ima-1)+zi(nbmama-1+ima-1)
    end do
!
! ------------------------ JEVEUX ------------------------------------
!
    call jedetr('&&FETSKP.NBMAMA')
    call wkvect('&&FETSKP.TEMP', 'V V I', max(1,maxi), temp)
    call wkvect('&&FETSKP.TEMP1', 'V V I', max(1,maxi), temp1)
!

    call wkvect('&&FETSKP.RENUM2', 'V V I', nbmato, renum2)
    call wkvect('&&FETSKP.RENUM3', 'V V I', nbmato, renum3)
    do ima = 1, nbmato
        zi(renum2-1+ima)=ima
        zi(renum3-1+ima)=ima
    end do
!
! ------------------------ JEVEUX ------------------------------------
!
    call wkvect('&&FETSKP.NBMAMA', 'V V I', nbmato, nbmama)
    call wkvect('&&FETSKP.ID1', 'V V I', nbmato, id1)
!
! ------ ON COMPTE LES LIENS -----------------------------------------
!
    nblien=0
    do ima = 1, nbmato
        id=0
        idnoeu=zi(idno-1+zi(renum2-1+ima))
        typma1=zk8(typma-1+zi(renum2-1+ima))
        do ino = 1, zi(nbno-1+zi(renum2-1+ima))
            numno=zi(idnoeu-1+ino)
            do i = zi(idcoi-1+numno), zi(idcoi-1+numno+1)-1
                mail=zi(coi-1+i)
                mail=zi(renum3-1+mail)
                if (mail .le. ima) goto 23
                do j = 1, id
                    if (zi(temp-1+j) .eq. mail) then
                        zi(temp1-1+j)=zi(temp1-1+j)+1
                        goto 23
                    endif
                end do
                zi(temp+id)=mail
                zi(temp1+id)=1
                id=id+1
 23             continue
            end do
        end do
!
        do j = 1, id
            typma2=zk8(typma-1+zi(renum2-1+zi(temp-1+j)))
            if (zi(temp1-1+j) .eq. 1) then
                if (typma1 .eq. 'POI1    ') goto 61
                if (typma2 .eq. 'POI1    ') goto 61
                if (typma1 .eq. 'SEG2    ') goto 61
                if (typma2 .eq. 'SEG2    ') goto 61
            else if (zi(temp1-1+j) .eq. 2) then
                if (typma1 .eq. 'SEG2    ') goto 61
                if (typma2 .eq. 'SEG2    ') goto 61
                if (typma1 .eq. 'TRIA3   ') goto 61
                if (typma1 .eq. 'QUAD4   ') goto 61
                if (typma2 .eq. 'TRIA3   ') goto 61
                if (typma2 .eq. 'QUAD4   ') goto 61
            else if (zi(temp1-1+j) .eq. 3) then
                if (typma1 .eq. 'TRIA3   ') then
                    if (typma2 .eq. 'TETRA4  ') goto 61
                    if (typma2 .eq. 'PENTA6  ') goto 61
                    if (typma2 .eq. 'PYRAM5  ') goto 61
                else if (typma1 .eq. 'TETRA4  ') then
                    if (typma2 .eq. 'TRIA3   ') goto 61
                    if (typma2 .eq. 'TETRA4  ') goto 61
                    if (typma2 .eq. 'PENTA6  ') goto 61
                    if (typma2 .eq. 'PYRAM5  ') goto 61
                else if (typma1 .eq. 'PENTA6  ') then
                    if (typma2 .eq. 'PENTA6  ') goto 61
                    if (typma2 .eq. 'TRIA3   ') goto 61
                    if (typma2 .eq. 'TETRA4  ') goto 61
                    if (typma2 .eq. 'PYRAM5  ') goto 61
                else if (typma1 .eq. 'PYRAM5  ') then
                    if (typma2 .eq. 'TRIA3   ') goto 61
                    if (typma2 .eq. 'TETRA4  ') goto 61
                    if (typma2 .eq. 'PENTA6  ') goto 61
                    if (typma2 .eq. 'PYRAM5  ') goto 61
                endif
            else if (zi(temp1-1+j) .eq. 4) then
                if (typma1 .eq. 'QUAD4   ') then
                    if (typma2 .eq. 'PENTA6  ') goto 61
                    if (typma2 .eq. 'HEXA8   ') goto 61
                    if (typma2 .eq. 'PYRAM5  ') goto 61
                else if (typma1 .eq. 'PENTA6  ') then
                    if (typma2 .eq. 'PENTA6  ') goto 61
                    if (typma2 .eq. 'PYRAM5  ') goto 61
                    if (typma2 .eq. 'HEXA8   ') goto 61
                else if (typma1 .eq. 'HEXA8   ') then
                    if (typma2 .eq. 'HEXA8   ') goto 61
                    if (typma2 .eq. 'QUAD4   ') goto 61
                    if (typma2 .eq. 'PYRAM5  ') goto 61
                    if (typma2 .eq. 'PENTA6  ') goto 61
                else if (typma1 .eq. 'PYRAM5  ') then
                    if (typma2 .eq. 'QUAD4   ') goto 61
                    if (typma2 .eq. 'PENTA6  ') goto 61
                    if (typma2 .eq. 'HEXA8   ') goto 61
                    if (typma2 .eq. 'PYRAM5  ') goto 61
                endif
            endif
!
            goto 24
!
 61         continue
            zi(nbmama-1+ima)=zi(nbmama-1+ima)+1
            zi(nbmama-1+zi(temp-1+j))=zi(nbmama-1+zi(temp-1+j))+1
            nblien=nblien+2
!
 24         continue
        end do
    end do
!
! ------- ON RE-REMPLIT IDCO -----------------------------------------
!
    zi4(idco)=1
    do ima = 2, nbmato+1
        zi4(idco-1+ima)=zi4(idco-1+ima-1)+zi(nbmama-1+ima-1)
    end do
!
! ------ CREATION DES CONNECTIVITES DES MAILLES ( CO ) ---------------
!
    call wkvect('&&FETSKP.CO', 'V V S', max(1,nblien), co)
!
    do ima = 1, nbmato
        nbre=0
        idnoeu=zi(idno-1+zi(renum2-1+ima))
        typma1=zk8(typma-1+zi(renum2-1+ima))
        do ino = 1, zi(nbno-1+zi(renum2-1+ima))
            numno=zi(idnoeu-1+ino)
            do i = zi(idcoi-1+numno), zi(idcoi-1+numno+1)-1
                mail=zi(coi-1+i)
                mail=zi(renum3-1+mail)
                if (mail .le. ima) goto 29
                do j = 1, nbre
                    if (zi(temp-1+j) .eq. mail) then
                        zi(temp1-1+j)=zi(temp1-1+j)+1
                        goto 29
                    endif
                end do
                zi(temp+nbre)=mail
                zi(temp1+nbre)=1
                nbre=nbre+1
 29             continue
            end do
        end do
!
        do j = 1, nbre
            typma2=zk8(typma-1+zi(renum2-1+zi(temp-1+j)))
            if (zi(temp1-1+j) .eq. 1) then
                if (typma1 .eq. 'POI1    ') goto 31
                if (typma2 .eq. 'POI1    ') goto 31
                if (typma1 .eq. 'SEG2    ') goto 31
                if (typma2 .eq. 'SEG2    ') goto 31
            else if (zi(temp1-1+j) .eq. 2) then
                if (typma1 .eq. 'SEG2    ') goto 31
                if (typma2 .eq. 'SEG2    ') goto 31
                if (typma1 .eq. 'TRIA3   ') goto 31
                if (typma1 .eq. 'QUAD4   ') goto 31
                if (typma2 .eq. 'TRIA3   ') goto 31
                if (typma2 .eq. 'QUAD4   ') goto 31
            else if (zi(temp1-1+j) .eq. 3) then
                if (typma1 .eq. 'TRIA3   ') then
                    if (typma2 .eq. 'TETRA4  ') goto 31
                    if (typma2 .eq. 'PENTA6  ') goto 31
                    if (typma2 .eq. 'PYRAM5  ') goto 31
                else if (typma1 .eq. 'TETRA4  ') then
                    if (typma2 .eq. 'TETRA4  ') goto 31
                    if (typma2 .eq. 'PENTA6  ') goto 31
                    if (typma2 .eq. 'TRIA3   ') goto 31
                    if (typma2 .eq. 'PYRAM5  ') goto 31
                else if (typma1 .eq. 'PENTA6  ') then
                    if (typma2 .eq. 'PENTA6  ') goto 31
                    if (typma2 .eq. 'TRIA3   ') goto 31
                    if (typma2 .eq. 'TETRA4  ') goto 31
                    if (typma2 .eq. 'PYRAM5  ') goto 31
                else if (typma1 .eq. 'PYRAM5  ') then
                    if (typma2 .eq. 'TRIA3   ') goto 31
                    if (typma2 .eq. 'TETRA4  ') goto 31
                    if (typma2 .eq. 'PENTA6  ') goto 31
                    if (typma2 .eq. 'PYRAM5  ') goto 31
                endif
            else if (zi(temp1-1+j) .eq. 4) then
                if (typma1 .eq. 'QUAD4   ') then
                    if (typma2 .eq. 'PENTA6  ') goto 31
                    if (typma2 .eq. 'HEXA8   ') goto 31
                    if (typma2 .eq. 'PYRAM5  ') goto 31
                else if (typma1 .eq. 'PENTA6  ') then
                    if (typma2 .eq. 'PENTA6  ') goto 31
                    if (typma2 .eq. 'HEXA8   ') goto 31
                    if (typma2 .eq. 'PYRAM5  ') goto 31
                else if (typma1 .eq. 'HEXA8   ') then
                    if (typma2 .eq. 'HEXA8   ') goto 31
                    if (typma2 .eq. 'PENTA6  ') goto 31
                    if (typma2 .eq. 'QUAD4   ') goto 31
                    if (typma2 .eq. 'PYRAM5  ') goto 31
                else if (typma1 .eq. 'PYRAM5  ') then
                    if (typma2 .eq. 'QUAD4   ') goto 31
                    if (typma2 .eq. 'PENTA6  ') goto 31
                    if (typma2 .eq. 'HEXA8   ') goto 31
                    if (typma2 .eq. 'PYRAM5  ') goto 31
                endif
            endif
!
            goto 30
!
 31         continue
            id=zi4(idco-1+ima)+zi(id1-1+ima)
            id2=zi4(idco-1+zi(temp-1+j))+zi(id1-1+zi(temp-1+j))
            zi4(co-1+id2)=ima
            zi4(co-1+id)=zi(temp-1+j)
            zi(id1-1+ima)=zi(id1-1+ima)+1
            zi(id1-1+zi(temp-1+j))=zi(id1-1+zi(temp-1+j))+1
!
 30         continue
        end do
    end do
!
! ------------------------ JEVEUX ------------------------------------
    call jedetr('&&FETSKP.NBNO')
    call jedetr('&&FETSKP.IDNO')
    call jedetr('&&FETSKP.TYPMA')
    call jedetr('&&FETSKP.IDCOI')
    call jedetr('&&FETSKP.ID1')
    call jedetr('&&FETSKP.COI')
    call jedetr('&&FETSKP.TEMP')
    call jedetr('&&FETSKP.TEMP1')
!
    if (niv .ge. 2) then
        call uttcpu('CPU.CREACO', 'FIN', ' ')
        call uttcpr('CPU.CREACO', 7, tmps)
        write(ifm,*)'--- CONNECTIVITE DES MAILLES:',tmps(7)
        write(ifm,*)'  '
    endif
!
    call jedema()
end subroutine
