subroutine slegro(iunv, imod, datset)
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
!     =================
!A PRESUPER
!
!     ================================================================
!     !                                                              !
!     !  FONCTION: LECTURE SUR LE FICHIER UNIVERSEL ISSU DE SUPERTAB !
!     !            I-DEAS 4.0, 6.0 DOU 7.0  DES GROUPES DE NOEUDS    !
!     !            ET DE MAILLES PUIS ECRITURE SUR LE FICHIER MODELE !
!     !                                                              !
!     ================================================================
!     !                                                              !
!     !  ROUTINES APPELES : CODENT                                   !
!     !                          : IUNIFI (FONCTION)                 !
!     !                          : JJMMAA                            !
!     !                          : CODNOP                            !
!     !                                                              !
!     !  ROUTINE APPELANTE : PRESUP                                  !
!     !                                                              !
!     ================================================================
!
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/codnop.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jjmmaa.h"
#include "asterfort/lxlgut.h"
#include "asterfort/lxscan.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=24) :: valk(2)
!
!
!  --> DECLARATION VARIABLES LOCALES
!
    character(len=4) :: ct(3)
    character(len=1) :: prfnoe, prfmai
    character(len=80) :: cbuf
    character(len=8) :: chnode, chmail, chgrou, ngro8
    character(len=8) :: toto
    character(len=20) :: nomgro
    character(len=12) :: chenti, chnomi, chnoma, aut
    character(len=13) :: chlige, chlign
    character(len=80) :: chfogn, cval
    character(len=80) :: chfogm
    integer :: entcod(4), nument(4), numgro, nbenti, nblign
    integer :: nbnode, nbmail, nbnod8, nbmai8, iunv, imod
    integer :: nblit, nblie, nblif
    integer :: imi, ima
    integer :: nbmodu, nbtest, datset
    integer :: nbrlig
    real(kind=8) :: rval
    logical :: lwrit
!
!  --> DECLARATION INDICES DE BOUCLES
!
    integer :: i, j
!
!  ------------ FIN DECLARATION -------------
!
!  -->N  D'UNITE LOGIQUE ASSOCIE AUX FICHIERS
!-----------------------------------------------------------------------
    integer :: iclass, icol, ilong, ind, ival, jgrm, jgrn
!-----------------------------------------------------------------------
    call jemarq()
!
    prfnoe='N'
    prfmai='M'
    chfogn='%FORMAT=(1*NOM_DE_NOEUD)'
    chfogm='%FORMAT=(1*NOM_DE_MAILLE)'
    chnode = '        '
    chmail = '        '
    chenti='NBOBJ=      '
    chlign='NBLIGT=      '
    chlige='NBLIGE=      '
    chnomi='NUMIN=      '
    chnoma='NUMAX=      '
!
 1  continue
    lwrit = .true.
    read (iunv,'(A)') cbuf
    read (cbuf,'(I6)') ind
    if (ind .ne. -1) then
!
!  --> LECTURE SUR LE FICHIER UNIVERSEL DES GROUPES DE NOEUDS
!      ET/OU DE MAILLES
!
        if (datset .eq. 752) then
            read (cbuf,'(I10,40X,I10)') numgro,nbenti
            read (iunv,'(A)') nomgro
            nbrlig = 4
        else if (datset.eq.2417) then
            read (cbuf,'(I10,50X,I10)') numgro,nbenti
            read (iunv,'(A)') nomgro
            nbrlig = 4
            else if (datset.eq.2429.or.datset.eq.2430 .or.datset.eq.2432)&
        then
            read (cbuf,'(I10,60X,I10)') numgro,nbenti
            read (iunv,'(A)') nomgro
            nbrlig = 4
            else if ((datset.eq.2435).or.(datset.eq.2467) .or.(&
        datset.eq.2452).or.(datset.eq.2477)) then
            read (cbuf,'(I10,60X,I10)') numgro,nbenti
            read (iunv,'(A)') nomgro
            nbrlig = 2
        endif
        icol = 1
        ilong = lxlgut (nomgro)
        if (ilong .gt. 8) then
            call utmess('A', 'STBTRIAS_5', sk=nomgro)
        endif
        ngro8 = nomgro
        call lxscan(ngro8, icol, iclass, ival, rval,&
                    cval)
        if (iclass .ne. 3) then
            call utmess('A', 'STBTRIAS_6', sk=ngro8)
            if (nbenti .eq. 0) then
                goto 1
            else
                nblign = int(nbenti/nbrlig)
                do 145 i = 1, nblign
                    read(iunv,'(I3)')
145              continue
                if (nbenti .gt. (nbrlig*nblign)) read(iunv,'(I3)')
                goto 1
            endif
        else if (ilong.ne.ival) then
            valk(1) = nomgro
            valk(2) = ngro8(1:ival)
            call utmess('A', 'STBTRIAS_7', nk=2, valk=valk)
            toto = ngro8(1:ival)
            ngro8 = ' '
            ngro8 = toto
        endif
        if (ngro8(1:5) .eq. 'COUL_') then
            call utmess('A', 'STBTRIAS_8')
            lwrit = .false.
        endif
        if (nbenti .eq. 0) goto 1
        call wkvect('&&PRESUP.GROU.NOEUD', 'V V K8', nbenti, jgrn)
        call wkvect('&&PRESUP.GROU.MAILLE', 'V V K8', nbenti, jgrm)
!
        nbnode=0
        nbmail=0
        nbtest=0
!
        nblign = int(nbenti/nbrlig)
        nbmodu = mod(nbenti,nbrlig)
!
        if (nbmodu .ne. 0) then
            nbtest=1
        endif
!
        do 2 i = 1, nblign
!
            if (datset .eq. 752 .or. datset .eq. 2417 .or. datset .eq. 2429 .or. datset&
                .eq. 2430 .or. datset .eq. 2432) then
                read (iunv,'(8I10)') (entcod(j),nument(j),j=1,nbrlig)
                elseif((datset.eq.2435).or.(datset.eq.2467) .or.(&
            datset.eq.2452).or.(datset.eq.2477)) then
                read (iunv,'(2(I10,I10,20X))') (entcod(j),nument(j),j=&
                1,nbrlig)
            endif
!
            do 3 j = 1, nbrlig
                if (entcod(j) .eq. 7) then
                    call codnop(chnode, prfnoe, 1, 1)
                    call codent(nument(j), 'G', chnode(2:8))
! --> RECHERCHE DU N MIN ET N MAX DANS UN GROUPE DE NOEUDS
!
                    if (nbnode .eq. 0) then
                        imi=nument(j)
                    else
                        ima=max(nument(j),imi)
                    endif
!
                    nbnode= nbnode+ 1
                    zk8(jgrn-1+nbnode) = chnode
                else if (entcod(j).eq.8) then
                    call codnop(chmail, prfmai, 1, 1)
                    call codent(nument(j), 'G', chmail(2:8))
! --> RECHERCHE DU N MIN ET N MAX DANS UN GROUPE DE MAILLES
!
                    if (nbmail .eq. 0) then
                        imi=nument(j)
                    else
                        ima=max(nument(j),imi)
                    endif
!
                    nbmail= nbmail+ 1
                    zk8(jgrm-1+nbmail) = chmail
                endif
 3          continue
!
 2      continue
!
        if (nbenti .gt. (nbrlig*nblign)) then
            if (datset .eq. 752 .or. datset .eq. 2417 .or. datset .eq. 2429 .or. datset&
                .eq. 2430 .or. datset .eq. 2432) then
                read (iunv,'(8I10)') (entcod(j),nument(j),j=1,nbrlig)
                elseif ((datset.eq.2435).or.(datset.eq.2467) .or.(&
            datset.eq.2452).or.(datset.eq.2477)) then
                read (iunv,'(2(I10,I10,20X))') (entcod(j),nument(j),j=&
                1,nbrlig)
            endif
            do 4 j = 1, (nbenti-nbrlig*nblign)
                if (entcod(j) .eq. 7) then
!
! --> ECRITURE DES NOEUDS (APPARTENANT A UN GROUPE) SUR
!     LE FICHIER BUFFER IGRN
!
                    call codnop(chnode, prfnoe, 1, 1)
                    call codent(nument(j), 'G', chnode(2:8))
                    if (nblign .eq. 0 .and. j .eq. 1) then
                        imi=nument(j)
                    endif
                    ima=max(nument(j),imi)
                    nbnode= nbnode+ 1
                    zk8(jgrn-1+nbnode) = chnode
                else if (entcod(j).eq.8) then
!
! --> ECRITURE DES MAILLES (APPARTENANT A UN GROUPE) SUR
!     LE FICHIER BUFFER IGRM
!
                    call codnop(chmail, prfmai, 1, 1)
                    call codent(nument(j), 'G', chmail(2:8))
                    if (nblign .eq. 0 .and. j .eq. 1) then
                        imi=nument(j)
                    endif
                    ima=max(nument(j),imi)
                    nbmail= nbmail+ 1
                    zk8(jgrm-1+nbmail) = chmail
                endif
 4          continue
        endif
!
! --> ECRITURE SUR LE FICHIER NEUTRE DES GROUPES DE NOEUDS
!
        if (nbnode .ne. 0) then
            chgrou(1:4)='GRNO'
            call codent(numgro, 'G', chgrou(5:8))
!
            nblie=3
            nblif=1
            nbnod8=nbnode/8
            nblit=nbnod8+nblie+nblif+nbtest+1
!
            call codent(nblit, 'G', chlign(8:13))
            call codent(nbnode, 'G', chenti(7:12))
            call codent(nblie, 'G', chlige(8:13))
            call codent(imi, 'G', chnomi(7:12))
            call codent(ima, 'G', chnoma(7:12))
!
!
!   --> ECRITURE DE LA DATE (IBM & CRAY)
            call jjmmaa(ct, aut)
!
            if (nomgro(1:9) .eq. 'PERMANENT') then
                write(imod,'(A,4X,2A,1X,A,1X,A,1X,A)')'GROUP_NO','NOM=',&
     &                 chgrou,chenti,chlige,chlign
            else
                write(imod,'(A,4X,2A,2X,A,1X,A,1X,A)')'GROUP_NO','NOM=',&
     &                 ngro8,chenti,chlige,chlign
            endif
!
            write(imod,'(12X,A,14X,A)') chnomi,chnoma
            write(imod,'(12X,2A,7X,A,A2,A,A2,A,A4)') 'AUTEUR=',aut,&
            'DATE=',ct(1)(1:2),'/',ct(2)(1:2),'/',ct(3)
            write(imod,'(A)') chfogn
!
! --> ECRITURE DES NOEUDS
!
            write (imod,'(8(2X,A))') (zk8(jgrn-1+j),j=1,nbnode)
!
! --> FIN ECRITURE DES NOEUDS
!
            write (imod,'(A)') 'FINSF'
            write (imod,'(A)') '%'
        endif
!
! --> ECRITURE SUR LE FICHIER NEUTRE DES GROUPES DE MAILLES
!
        if (nbmail .ne. 0) then
            chgrou(1:4)='GRMA'
            call codent(numgro, 'G', chgrou(5:8))
!
            nblie=3
            nblif=1
            nbmai8=nbmail/8
            nblit=nbmai8+nblie+nblif+nbtest+1
!
            call codent(nbmail, 'G', chenti(7:12))
            call codent(nblit, 'G', chlign(8:13))
            call codent(nblie, 'G', chlige(8:13))
            call codent(imi, 'G', chnomi(7:12))
            call codent(ima, 'G', chnoma(7:12))
!
!   --> ECRITURE DE LA DATE (IBM & CRAY)
            call jjmmaa(ct, aut)
!
            if (nomgro(1:9) .eq. 'PERMANENT' .and. lwrit) then
                write(imod,'(A,4X,2A,1X,A,1X,A,1X,A)')'GROUP_MA','NOM=',&
     &               chgrou,chenti,chlige,chlign
            else if (lwrit) then
                write(imod,'(A,4X,2A,2X,A,1X,A,1X,A)')'GROUP_MA','NOM=',&
     &               ngro8,chenti,chlige,chlign
            endif
            if (lwrit) write(imod,'(12X,A,14X,A)') chnomi,chnoma
            if (lwrit) write(imod, '(12X,2A,7X,A,A2,A,A2,A,A4)') 'AUTEUR=', aut, 'DATE=',&
                       ct(1)(1:2), '/', ct(2)(1:2), '/', ct(3)
            if (lwrit) write(imod,'(A)') chfogm
!
! --> ECRITURE DES MAILLES
!
            if (lwrit) write (imod,'(8(2X,A))') (zk8(jgrm-1+j),j=1, nbmail)
!
! --> FIN ECRITURE DES MAILLES
!
            if (lwrit) write (imod,'(A)') 'FINSF'
            if (lwrit) write (imod,'(A)') '%'
        endif
        call jedetr('&&PRESUP.GROU.NOEUD')
        call jedetr('&&PRESUP.GROU.MAILLE')
        goto 1
    endif
    call jedema()
end subroutine
