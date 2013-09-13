subroutine slegeo(iunv, imod)
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
!
!  ================================================================
!  !                                                              !
!  !  FONCTION : LECTURE DANS LE FICHIER UNIVERSEL ISSU DE SUPER- !
!  !             TAB I-DEAS 4.0, 6.0 OU 7.0   DES NOEUDS ET       !
!  !             MAILLES RATTACHES AUX CURVES,MESHS AREA ET MESHS !
!  !             VOLUME PUIS ECRITURE DANS LE FICHIER MODELE.DANS !
!  !             UN SECOND TEMPS REGROUPEMENT DES CURVES,DES MESH !
!  !             ET DES MESH VOLUMES PUIS ECRITURE DANS LE FICHIER!
!  !             MODELE                                           !
!  !                                                              !
!  ================================================================
!  !                                                              !
!  !  ROUTINES APPELES : CODENT                                   !
!  !                          : IUNIFI (FONCTION)                 !
!  !                          : JJMMAA                            !
!  !                          : ECRCAV                            !
!  !                                                              !
!  !  ROUTINE APPELANTE : PRESUP                                  !
!  !                                                              !
!  ================================================================
!
!
!  --> DECLARATION DES VARIABLES LOCALES
!
#include "asterfort/codent.h"
#include "asterfort/codnop.h"
#include "asterfort/iunifi.h"
#include "asterfort/jjmmaa.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
    character(len=1) :: prfnoe, prfmai
    character(len=80) :: cbuf
    character(len=4) :: ct(3)
    character(len=8) :: chmail, chnode, nuobj(8)
    character(len=10) :: nomc, noma, nomv
    character(len=12) :: chenti, aut
    character(len=13) :: chlign, chlige
    character(len=80) :: chfogn
    character(len=80) :: chfogm
    integer :: ind, indic, inum, ityp, nbenti, itest, nblign
    integer :: nblit, nblie, nblif
    integer :: icompc, icompa, icompv
    integer :: nument(8)
    integer :: iunv, imes
!
!  --> DECLARATION INDICE DE BOUCLES
!
    integer :: i, j, k
!  ------------ FIN DECLARATION ---------------
!
!  --> N  D'UNITE LOGIQUE ASSOCIE AUX FICHIERS
!-----------------------------------------------------------------------
    integer :: idiv, ilong, imod, irest
!-----------------------------------------------------------------------
    imes=iunifi('MESSAGE')
!
!  --> COMPTEUR :NBRE DE CURVES,NBRE DE M.AREA,NBRE DE M.VOLUME
!
    icompc=0
    icompa=0
    icompv=0
!
    prfnoe='N'
    prfmai='M'
    chfogn='%FORMAT=(1*NOM_DE_NOEUD)'
    chfogm='%FORMAT=(1*NOM_DE_MAILLE)'
    chnode='        '
    chmail='        '
    chenti='NBOBJ=      '
    chlign='NBLIGT=      '
    chlige='NBLIGE=      '
!
 1  continue
    read(iunv,'(A)')cbuf
    read(cbuf,'(4X,I2)') ind
    nblign=0
    itest=0
    if (ind .eq. -1) goto 1000
    read(cbuf,'(4I10)') indic,inum,ityp,nbenti
    if (indic .eq. 3 .or. indic .eq. 5 .or. indic .eq. 6) then
!
        if (indic .eq. 3 .and. ityp .eq. 7) then
!         --> ON TROUVE UNE CURVE DECRITE PAR SES NOEUDS
            icompc=icompc+1
            nomc(1:5)='CURVE'
            call codent(inum, 'G', nomc(6:10))
!
        else if (indic.eq.3.and.ityp.eq.8) then
!         --> ON TROUVE UNE CURVE DECRITE PAR SES ELEMENTS
!
        else if (indic.eq.5.and.ityp.eq.7) then
!         --> ON TROUVE UNE MESH AREA DECRITE PAR SES NOEUDS
            icompa=icompa+1
            noma(1:6)='M_AREA'
            call codent(inum, 'G', noma(7:10))
!
        else if (indic.eq.5.and.ityp.eq.8) then
!         --> ON TROUVE UNE MESH AREA DECRITE PAR SES ELEMENTS
!
        else if (indic.eq.6.and.ityp.eq.7) then
!         --> ON TROUVE UN MESH VOLUME DECRIT PAR SES POINTS
            icompv=icompv+1
            nomv(1:6)='M_VOLU'
            call codent(inum, 'G', nomv(7:10))
!
        else
!         --> ON TROUVE UN MESH VOLUME DECRIT PAR SES ELEMENTS
        endif
!
        idiv=int(nbenti/8)
        irest=mod(nbenti,8)
!
        if (irest .ne. 0) then
            itest=1
        endif
!
        nblign=idiv+itest
        nblie=2
        nblif=1
        nblit=nblign+nblie+nblif+1
!
        call codent(nbenti, 'G', chenti(7:12))
        call codent(nblit, 'G', chlign(8:13))
        call codent(nblie, 'G', chlige(8:13))
!
!   --> ECRITURE DE LA DATE(IBM & CRAY)
        call jjmmaa(ct, aut)
!
        if (indic .eq. 3 .and. ityp .eq. 7) then
!
!    ---> ON TROUVE UNE CURVE DECRITE PAR SES NOEUDS
!
            ilong=lxlgut(nomc)
            if (ilong .gt. 8) then
                call utmess('A', 'STBTRIAS_5', sk=nomc)
            endif
            write(imod,'(A,4X,2A,2X,A,1X,A,1X,A)') 'GROUP_NO','NOM=',&
            nomc(1:8),chenti,chlige,chlign
            write(imod,'(12X,2A,10X,A,A2,A,A2,A,A4)')'AUTEUR=',aut,&
            'DATE=',ct(1)(1:2),'/',ct(2)(1:2),'/',ct(3)
            write(imod,'(A)') chfogn
        else if (indic.eq.5.and.ityp.eq.7) then
!
!    ---> ON TROUVE UNE MESH AREA DECRITE PAR SES NOEUDS
!
            ilong=lxlgut(noma)
            if (ilong .gt. 8) then
                call utmess('A', 'STBTRIAS_5', sk=noma)
            endif
            write(imod,'(A,4X,2A,2X,A,1X,A,1X,A)') 'GROUP_NO','NOM=',&
            noma(1:8),chenti,chlige,chlign
            write(imod,'(12X,2A,10X,A,A2,A,A2,A,A2)') 'AUTEUR=',aut,&
            'DATE=',ct(1),'/',ct(2),'/',ct(3)
            write(imod,'(A)') chfogn
        else if (indic.eq.6.and.ityp.eq.7) then
!
!    ---> ON TROUVE UN MESH VOLUME DECRIT PAR SES NOEUDS
!
            ilong=lxlgut(nomv)
            if (ilong .gt. 8) then
                call utmess('A', 'STBTRIAS_5', sk=nomv)
            endif
            write(imod,'(A,4X,2A,2X,A,1X,A,1X,A)') 'GROUP_NO','NOM=',&
            nomv(1:8),chenti,chlige,chlign
            write(imod,'(12X,2A,10X,A,A2,A,A2,A,A2)') 'AUTEUR=',aut,&
            'DATE=',ct(1),'/',ct(2),'/',ct(3)
            write(imod,'(A)') chfogn
        else if (indic.eq.3.and.ityp.eq.8) then
!
!    ---> ON TROUVE UNE CURVE DECRITE PAR SES ELEMENTS
!
            ilong=lxlgut(nomc)
            if (ilong .gt. 8) then
                call utmess('A', 'STBTRIAS_5', sk=nomc)
            endif
            write(imod,'(A,4X,2A,2X,A,1X,A,1X,A)')'GROUP_MA','NOM=',&
            nomc(1:8),chenti,chlige,chlign
            write(imod,'(12X,2A,10X,A,A2,A,A2,A,A2)') 'AUTEUR=',aut,&
            'DATE=',ct(1),'/',ct(2),'/',ct(3)
            write(imod,'(A)') chfogm
        else if (indic.eq.5.and.ityp.eq.8) then
!
!    ---> ON TROUVE UNE MESH AREA DECRITE PAR SES ELEMENTS
!
            ilong=lxlgut(noma)
            if (ilong .gt. 8) then
                call utmess('A', 'STBTRIAS_5', sk=nomc)
            endif
            write(imod,'(A,4X,2A,2X,A,1X,A,1X,A)')'GROUP_MA','NOM=',&
            noma(1:8),chenti,chlige,chlign
            write(imod,'(12X,2A,10X,A,A2,A,A2,A,A2)') 'AUTEUR=',aut,&
            'DATE:',ct(1),'/',ct(2),'/',ct(3)
            write(imod,'(A)') chfogm
        else
!
!    ---> ON TROUVE UN MESH VOLUME DECRIT PAR DES ELEMENTS
!
            ilong=lxlgut(nomv)
            if (ilong .gt. 8) then
                call utmess('A', 'STBTRIAS_5', sk=nomv)
            endif
            write(imod,'(A,4X,2A,2X,A,1X,A,1X,A)') 'GROUP_MA','NOM=',&
            nomv(1:8),chenti,chlige,chlign
            write(imod,'(12X,2A,10X,A,A2,A,A2,A,A2)') 'AUTEUR=',aut,&
            'DATE=',ct(1),'/',ct(2),'/',ct(3)
            write(imod,'(A)') chfogm
        endif
        if (ityp .eq. 7) then
            if (idiv .ne. 0) then
                do 20 i = 1, idiv
                    read(iunv,'(8I10)') (nument(k),k=1,8)
                    do 25 k = 1, 8
                        call codnop(chnode, prfnoe, 1, 1)
                        call codent(nument(k), 'G', chnode(2:8))
                        nuobj(k)=chnode
25                  continue
                    write(imod,'(8(2X,A))') (nuobj(j),j=1,8)
20              continue
            endif
!
            if (irest .ne. 0) then
                read (iunv,'(8I10)') (nument(i),i=1,8)
                do 30 k = 1, irest
                    call codnop(chnode, prfnoe, 1, 1)
                    call codent(nument(k), 'G', chnode(2:8))
                    nuobj(k)=chnode
30              continue
                write(imod,'(8(2X,A))') (nuobj(j),j=1,irest)
            endif
            write(imod,'(A)') 'FINSF'
            write(imod,'(A)') '%'
!
        else
!
            if (idiv .ne. 0) then
                do 40 i = 1, idiv
                    read(iunv,'(8I10)') (nument(k),k=1,8)
                    do 45 k = 1, 8
                        call codnop(chmail, prfmai, 1, 1)
                        call codent(nument(k), 'G', chmail(2:8))
                        nuobj(k)=chmail
45                  continue
                    write(imod,'(8(2X,A))') (nuobj(j),j=1,8)
40              continue
            endif
!
            if (irest .ne. 0) then
                read (iunv,'(8I10)') (nument(i),i=1,8)
                do 50 k = 1, irest
                    call codnop(chmail, prfmai, 1, 1)
                    call codent(nument(k), 'G', chmail(2:8))
                    nuobj(k)=chmail
50              continue
                write(imod,'(8(2X,A))') (nuobj(j),j=1,irest)
            endif
            write(imod,'(A)') 'FINSF'
            write(imod,'(A)') '%'
        endif
    else
        write(imes,*) 'ON EST DANS 1 AUBERGE ESPAGNOLE'
        write(imes,*) 'JE N''Y COMPRENDS PLUS RIEN'
    endif
    goto 1
1000  continue
    write(imes,*) 'NOMBRE DE CURVES :',icompc
    write(imes,*) 'NOMBRE DE M. AREAS:',icompa
    write(imes,*) 'NOMBRE DE M. VOLUMES:',icompv
!
end subroutine
