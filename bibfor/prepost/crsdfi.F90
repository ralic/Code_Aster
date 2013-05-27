subroutine crsdfi(linoch, nbnoch, noidez)
    implicit none
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterc/getvis.h'
    include 'asterc/getvtx.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: linoch(1), noidez
    integer :: nbnoch
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!----------------------------------------------------------------------
!
!     CREATION DE LA SD "FORMAT_IDEAS"
!
! IN  : LINOCH : L_K16 : LISTE DES NOMS DE CHAMP ('DEPL','SIEF_ELNO')
! IN  : NBNOCH : I     : NOMBRE DE CHAMPS A LIRE
! IN  : NOIDEZ : K16   : NOM DE LA SD FORMAT_IDEAS
! IN  : NBVARI : I     : NOMBRE DE VARIABLES INTERNES A LIRE POUR LE
!                        CHAMP DE VARIABLES INTERNES (VARI_R)
!
!----------------------------------------------------------------------
!
    character(len=16) :: param
!
!
    integer :: posi(2), nposi
    integer :: lfinom, lfinum, lfipar, lfiloc, lfinbc, lficmp
    integer :: iocc, nbocc, nval, nch, rec(20)
    integer :: i, ich, ival, nb, nbcmp
    character(len=8) :: cmp(1000)
    character(len=16) :: blanc, nocham, noidea
    integer :: iarg
    data blanc/'                '/
!
    call jemarq()
!
!- CREATION DE LA SRUCTURE DE DONNEES "FORMAT_IDEAS"
!
    noidea = noidez
    call wkvect(noidea//'.FID_NOM', 'V V K16', nbnoch, lfinom)
    call wkvect(noidea//'.FID_NUM', 'V V I', nbnoch, lfinum)
    call wkvect(noidea//'.FID_PAR', 'V V I', nbnoch*800, lfipar)
    call wkvect(noidea//'.FID_LOC', 'V V I', nbnoch*12, lfiloc)
    call wkvect(noidea//'.FID_CMP', 'V V K8', nbnoch*1000, lficmp)
    call wkvect(noidea//'.FID_NBC', 'V V I', nbnoch, lfinbc)
!
!- INITIALISATION DES OBJETS :
!           '.FID_NOM' , '.FID_NUM'
!           '.FID_PAR' , '.FID_CMP'
!
    do 10 i = 1, nbnoch
        zk16(lfinom-1+i) = blanc
10  end do
    do 20 i = 1, nbnoch
        zi(lfinum-1+i) = 9999
20  end do
    do 30 i = 1, 800*nbnoch
        zi(lfipar-1+i) = 9999
30  end do
!
    call getfac('FORMAT_IDEAS', nbocc)
!--------------------------------------------------------------------
!
!- REMPLISSAGE DES OBJETS A PARTIR DES DONNEES UTILISATEUR
!
!--------------------------------------------------------------------
    if (nbocc .ne. 0) then
!
        do 90 ich = 1, nbnoch
!
            do 80 iocc = 1, nbocc
                call getvtx('FORMAT_IDEAS', 'NOM_CHAM', iocc, iarg, 1,&
                            nocham, nch)
                if (nocham .eq. linoch(ich)) then
!- NOM_CHAM
                    zk16(lfinom-1+ich) = linoch(ich)
!- NUME_DATASET
                    call getvis('FORMAT_IDEAS', 'NUME_DATASET', iocc, iarg, 1,&
                                ival, nval)
                    if (nval .ne. 0) then
                        zi(lfinum-1+ich) = ival
                    endif
!- RECORD 3
                    call getvis('FORMAT_IDEAS', 'RECORD_3', iocc, iarg, 0,&
                                rec, nb)
                    nval = -nb
                    call getvis('FORMAT_IDEAS', 'RECORD_3', iocc, iarg, nval,&
                                rec, nb)
                    if (nval .ne. 0) then
                        do 40 i = 1, nval
                            zi(lfipar-1+ (ich-1)*800+80+i) = rec(i)
40                      continue
                    endif
!- RECORD 6
                    call getvis('FORMAT_IDEAS', 'RECORD_6', iocc, iarg, 0,&
                                rec, nb)
                    nval = -nb
                    call getvis('FORMAT_IDEAS', 'RECORD_6', iocc, iarg, nval,&
                                rec, nb)
                    if (nval .ne. 0) then
                        do 50 i = 1, nval
                            zi(lfipar-1+ (ich-1)*800+200+i) = rec(i)
50                      continue
                    endif
!- RECORD 9
                    call getvis('FORMAT_IDEAS', 'RECORD_9', iocc, iarg, 0,&
                                rec, nb)
                    nval = -nb
                    call getvis('FORMAT_IDEAS', 'RECORD_9', iocc, iarg, nval,&
                                rec, nb)
                    if (nval .ne. 0) then
                        do 60 i = 1, nval
                            zi(lfipar-1+ (ich-1)*800+320+i) = rec(i)
60                      continue
                    endif
!- POSI_ORDRE
                    param='POSI_ORDRE'
                    call getvis('FORMAT_IDEAS', param, iocc, iarg, 2,&
                                posi, nposi)
                    zi(lfiloc-1+ (ich-1)*12+1) = posi(1)
                    zi(lfiloc-1+ (ich-1)*12+2) = posi(2)
!- POSI_INST
                    param='POSI_INST'
                    call getvis('FORMAT_IDEAS', param, iocc, iarg, 2,&
                                posi, nposi)
                    if (nposi .eq. 2) then
                        zi(lfiloc-1+ (ich-1)*12+3) = posi(1)
                        zi(lfiloc-1+ (ich-1)*12+4) = posi(2)
                    endif
!- POSI_FREQ
                    param='POSI_FREQ'
                    call getvis('FORMAT_IDEAS', param, iocc, iarg, 2,&
                                posi, nposi)
                    if (nposi .eq. 2) then
                        zi(lfiloc-1+ (ich-1)*12+5) = posi(1)
                        zi(lfiloc-1+ (ich-1)*12+6) = posi(2)
                    endif
!- POSI_NUME_MODE
                    param='POSI_NUME_MODE'
                    call getvis('FORMAT_IDEAS', param, iocc, iarg, 2,&
                                posi, nposi)
                    if (nposi .eq. 2) then
                        zi(lfiloc-1+ (ich-1)*12+7) = posi(1)
                        zi(lfiloc-1+ (ich-1)*12+8) = posi(2)
                    endif
!- POSI_MASS_GENE
                    param='POSI_MASS_GENE'
                    call getvis('FORMAT_IDEAS', param, iocc, iarg, 2,&
                                posi, nposi)
                    if (nposi .eq. 2) then
                        zi(lfiloc-1+ (ich-1)*12+9) = posi(1)
                        zi(lfiloc-1+ (ich-1)*12+10) = posi(2)
                    endif
!- POSI_AMOR_GENE
                    param='POSI_AMOR_GENE'
                    call getvis('FORMAT_IDEAS', param, iocc, iarg, 2,&
                                posi, nposi)
                    if (nposi .eq. 2) then
                        zi(lfiloc-1+ (ich-1)*12+11) = posi(1)
                        zi(lfiloc-1+ (ich-1)*12+12) = posi(2)
                    endif
!
!
!- CMP ET NOMBRE DE COMPOSANTES
                    call getvtx('FORMAT_IDEAS', 'NOM_CMP', iocc, iarg, 0,&
                                cmp, nb)
                    nbcmp = -nb
                    call getvtx('FORMAT_IDEAS', 'NOM_CMP', iocc, iarg, nbcmp,&
                                cmp, nb)
                    if (nb .ne. 0) then
                        do 70 i = 1, nbcmp
                            zk8(lficmp-1+ (ich-1)*1000+i) = cmp(i)
70                      continue
                        zi(lfinbc-1+ich) = nbcmp
                    endif
                endif
80          continue
90      continue
    endif
!
!---------------------------------------------------------------------
!
!- REMPLISSAGE DES OBJETS A PARTIR DES VALEURS PAR DEFAUT
!
!---------------------------------------------------------------------
    do 100 ich = 1, nbnoch
        if (zk16(lfinom-1+ich) .eq. blanc) then
            nocham = linoch(ich)
!
!------------------------------- 'DEPL' --------------------------------
!
            if (nocham(1:4) .eq. 'DEPL') then
!- NOM_CHAM
                zk16(lfinom-1+ich) = nocham
!- NUME_DATASET
                zi(lfinum-1+ich) = 55
!- RECORD 6
                zi(lfipar-1+ (ich-1)*800+200+1) = 1
                zi(lfipar-1+ (ich-1)*800+200+3) = 3
                zi(lfipar-1+ (ich-1)*800+200+4) = 8
                zi(lfipar-1+ (ich-1)*800+200+5) = 2
                zi(lfipar-1+ (ich-1)*800+200+6) = 6
!- POSI_ORDRE
                zi(lfiloc-1+ (ich-1)*12+1) = 7
                zi(lfiloc-1+ (ich-1)*12+2) = 4
!- POSI_INST
                zi(lfiloc-1+ (ich-1)*12+3) = 8
                zi(lfiloc-1+ (ich-1)*12+4) = 1
!- POSI_FREQ
                zi(lfiloc-1+ (ich-1)*12+5) = 8
                zi(lfiloc-1+ (ich-1)*12+6) = 1
!- NOM_CMP
                zk8(lficmp-1+ (ich-1)*1000+1) = 'DX'
                zk8(lficmp-1+ (ich-1)*1000+2) = 'DY'
                zk8(lficmp-1+ (ich-1)*1000+3) = 'DZ'
                zk8(lficmp-1+ (ich-1)*1000+4) = 'DRX'
                zk8(lficmp-1+ (ich-1)*1000+5) = 'DRY'
                zk8(lficmp-1+ (ich-1)*1000+6) = 'DRZ'
                zi(lfinbc-1+ich) = 6
!
!------------------------------- 'VITE' --------------------------------
!
            else if (nocham(1:4).eq.'VITE') then
!- NOM_CHAM
                zk16(lfinom-1+ich) = nocham
!- NUME_DATASET
                zi(lfinum-1+ich) = 55
!- RECORD 6
                zi(lfipar-1+ (ich-1)*800+200+1) = 1
                zi(lfipar-1+ (ich-1)*800+200+3) = 3
                zi(lfipar-1+ (ich-1)*800+200+4) = 11
                zi(lfipar-1+ (ich-1)*800+200+5) = 2
                zi(lfipar-1+ (ich-1)*800+200+6) = 6
!- POSI_ORDRE
                zi(lfiloc-1+ (ich-1)*12+1) = 7
                zi(lfiloc-1+ (ich-1)*12+2) = 4
!- POSI_INST
                zi(lfiloc-1+ (ich-1)*12+3) = 8
                zi(lfiloc-1+ (ich-1)*12+4) = 1
!- POSI_FREQ
                zi(lfiloc-1+ (ich-1)*12+5) = 8
                zi(lfiloc-1+ (ich-1)*12+6) = 1
!- NOM_CMP
                zk8(lficmp-1+ (ich-1)*1000+1) = 'DX'
                zk8(lficmp-1+ (ich-1)*1000+2) = 'DY'
                zk8(lficmp-1+ (ich-1)*1000+3) = 'DZ'
                zk8(lficmp-1+ (ich-1)*1000+4) = 'DRX'
                zk8(lficmp-1+ (ich-1)*1000+5) = 'DRY'
                zk8(lficmp-1+ (ich-1)*1000+6) = 'DRZ'
!
                zi(lfinbc-1+ich) = 6
!
!------------------------------- 'ACCE' --------------------------------
!
            else if (nocham(1:4).eq.'ACCE') then
!- NOM_CHAM
                zk16(lfinom-1+ich) = nocham
!- NUME_DATASET
                zi(lfinum-1+ich) = 55
!- RECORD 6
                zi(lfipar-1+ (ich-1)*800+200+1) = 1
                zi(lfipar-1+ (ich-1)*800+200+3) = 3
                zi(lfipar-1+ (ich-1)*800+200+4) = 12
                zi(lfipar-1+ (ich-1)*800+200+5) = 2
                zi(lfipar-1+ (ich-1)*800+200+6) = 6
!- POSI_ORDRE
                zi(lfiloc-1+ (ich-1)*12+1) = 7
                zi(lfiloc-1+ (ich-1)*12+2) = 4
!- POSI_INST
                zi(lfiloc-1+ (ich-1)*12+3) = 8
                zi(lfiloc-1+ (ich-1)*12+4) = 1
!- POSI_FREQ
                zi(lfiloc-1+ (ich-1)*12+5) = 8
                zi(lfiloc-1+ (ich-1)*12+6) = 1
!- NOM_CMP
                zk8(lficmp-1+ (ich-1)*1000+1) = 'DX'
                zk8(lficmp-1+ (ich-1)*1000+2) = 'DY'
                zk8(lficmp-1+ (ich-1)*1000+3) = 'DZ'
                zk8(lficmp-1+ (ich-1)*1000+4) = 'DRX'
                zk8(lficmp-1+ (ich-1)*1000+5) = 'DRY'
                zk8(lficmp-1+ (ich-1)*1000+6) = 'DRZ'
!
                zi(lfinbc-1+ich) = 6
!
!------------------------------- 'TEMP' --------------------------------
!
            else if (nocham(1:4).eq.'TEMP') then
!- NOM_CHAM
                zk16(lfinom-1+ich) = nocham
!- NUME_DATASET
                zi(lfinum-1+ich) = 55
!- RECORD 6
                zi(lfipar-1+ (ich-1)*800+200+1) = 2
                zi(lfipar-1+ (ich-1)*800+200+2) = 4
                zi(lfipar-1+ (ich-1)*800+200+3) = 1
                zi(lfipar-1+ (ich-1)*800+200+4) = 5
                zi(lfipar-1+ (ich-1)*800+200+5) = 2
                zi(lfipar-1+ (ich-1)*800+200+6) = 1
!- POSI_ORDRE
                zi(lfiloc-1+ (ich-1)*12+1) = 7
                zi(lfiloc-1+ (ich-1)*12+2) = 4
!- POSI_INST
                zi(lfiloc-1+ (ich-1)*12+3) = 8
                zi(lfiloc-1+ (ich-1)*12+4) = 1
!- POSI_FREQ
                zi(lfiloc-1+ (ich-1)*12+5) = 8
                zi(lfiloc-1+ (ich-1)*12+6) = 1
!- NOM_CMP
                zk8(lficmp-1+ (ich-1)*1000+1) = 'TEMP'
!
                zi(lfinbc-1+ich) = 1
!
!------------------------------- 'VARI_ELNO' ------------------------
!
            else if (nocham(1:4).eq.'VARI') then
!- NOM_CHAM
                zk16(lfinom-1+ich) = nocham
!- NUME_DATASET
                zi(lfinum-1+ich) = 57
!- RECORD 6
                zi(lfipar-1+ (ich-1)*800+200+1) = 1
                zi(lfipar-1+ (ich-1)*800+200+2) = 4
                zi(lfipar-1+ (ich-1)*800+200+3) = 3
                zi(lfipar-1+ (ich-1)*800+200+4) = 0
                zi(lfipar-1+ (ich-1)*800+200+5) = 2
                zi(lfipar-1+ (ich-1)*800+200+6) = 6
!- POSI_ORDRE
                zi(lfiloc-1+ (ich-1)*12+1) = 7
                zi(lfiloc-1+ (ich-1)*12+2) = 4
!- POSI_INST
                zi(lfiloc-1+ (ich-1)*12+3) = 8
                zi(lfiloc-1+ (ich-1)*12+4) = 1
!- POSI_FREQ
                zi(lfiloc-1+ (ich-1)*12+5) = 8
                zi(lfiloc-1+ (ich-1)*12+6) = 1
!- NOM_CMP
                zk8(lficmp-1+ (ich-1)*1000+1) = 'V1'
                zk8(lficmp-1+ (ich-1)*1000+2) = 'V2'
                zk8(lficmp-1+ (ich-1)*1000+3) = 'V3'
                zk8(lficmp-1+ (ich-1)*1000+4) = 'V4'
                zk8(lficmp-1+ (ich-1)*1000+5) = 'V5'
                zk8(lficmp-1+ (ich-1)*1000+6) = 'V6'
                zk8(lficmp-1+ (ich-1)*1000+7) = 'V7'
                zk8(lficmp-1+ (ich-1)*1000+8) = 'V8'
                zk8(lficmp-1+ (ich-1)*1000+9) = 'V9'
                zk8(lficmp-1+ (ich-1)*1000+10) = 'V10'
                zk8(lficmp-1+ (ich-1)*1000+11) = 'V11'
                zk8(lficmp-1+ (ich-1)*1000+12) = 'V12'
                zk8(lficmp-1+ (ich-1)*1000+13) = 'V13'
                zk8(lficmp-1+ (ich-1)*1000+14) = 'V14'
                zk8(lficmp-1+ (ich-1)*1000+15) = 'V15'
                zk8(lficmp-1+ (ich-1)*1000+16) = 'V16'
                zk8(lficmp-1+ (ich-1)*1000+17) = 'V17'
                zk8(lficmp-1+ (ich-1)*1000+18) = 'V18'
                zk8(lficmp-1+ (ich-1)*1000+19) = 'V19'
                zk8(lficmp-1+ (ich-1)*1000+20) = 'V20'
                zk8(lficmp-1+ (ich-1)*1000+21) = 'V21'
                zk8(lficmp-1+ (ich-1)*1000+22) = 'V22'
                zk8(lficmp-1+ (ich-1)*1000+23) = 'V23'
                zk8(lficmp-1+ (ich-1)*1000+24) = 'V24'
                zk8(lficmp-1+ (ich-1)*1000+25) = 'V25'
                zk8(lficmp-1+ (ich-1)*1000+26) = 'V26'
                zk8(lficmp-1+ (ich-1)*1000+27) = 'V27'
                zk8(lficmp-1+ (ich-1)*1000+28) = 'V28'
                zk8(lficmp-1+ (ich-1)*1000+29) = 'V29'
                zk8(lficmp-1+ (ich-1)*1000+30) = 'V30'
!
                zi(lfinbc-1+ich) = 30
!
!------------------------------- 'EPSA_ELNO' ------------------------
!
            else if (nocham(1:4).eq.'EPSA') then
!- NOM_CHAM
                zk16(lfinom-1+ich) = nocham
!- NUME_DATASET
                zi(lfinum-1+ich) = 57
!- RECORD 6
                zi(lfipar-1+ (ich-1)*800+200+1) = 1
                zi(lfipar-1+ (ich-1)*800+200+2) = 4
                zi(lfipar-1+ (ich-1)*800+200+3) = 4
                zi(lfipar-1+ (ich-1)*800+200+4) = 3
                zi(lfipar-1+ (ich-1)*800+200+5) = 2
                zi(lfipar-1+ (ich-1)*800+200+6) = 6
!- POSI_ORDRE
                zi(lfiloc-1+ (ich-1)*12+1) = 7
                zi(lfiloc-1+ (ich-1)*12+2) = 4
!- POSI_INST
                zi(lfiloc-1+ (ich-1)*12+3) = 8
                zi(lfiloc-1+ (ich-1)*12+4) = 1
!- POSI_FREQ
                zi(lfiloc-1+ (ich-1)*12+5) = 8
                zi(lfiloc-1+ (ich-1)*12+6) = 1
!- NOM_CMP
                zk8(lficmp-1+ (ich-1)*1000+1) = 'EPXX'
                zk8(lficmp-1+ (ich-1)*1000+2) = 'EPXY'
                zk8(lficmp-1+ (ich-1)*1000+3) = 'EPYY'
                zk8(lficmp-1+ (ich-1)*1000+4) = 'EPXZ'
                zk8(lficmp-1+ (ich-1)*1000+5) = 'EPYZ'
                zk8(lficmp-1+ (ich-1)*1000+6) = 'EPZZ'
!
                zi(lfinbc-1+ich) = 6
!
!------------------------------- 'SIEF_ELNO' ------------------------
!
            else if (nocham(1:4).eq.'SIEF') then
!- NOM_CHAM
                zk16(lfinom-1+ich) = nocham
!- NUME_DATASET
                zi(lfinum-1+ich) = 57
!- RECORD 6
                zi(lfipar-1+ (ich-1)*800+200+1) = 1
                zi(lfipar-1+ (ich-1)*800+200+2) = 4
                zi(lfipar-1+ (ich-1)*800+200+3) = 4
                zi(lfipar-1+ (ich-1)*800+200+4) = 2
                zi(lfipar-1+ (ich-1)*800+200+5) = 2
                zi(lfipar-1+ (ich-1)*800+200+6) = 6
!- POSI_ORDRE
                zi(lfiloc-1+ (ich-1)*12+1) = 7
                zi(lfiloc-1+ (ich-1)*12+2) = 4
!- POSI_INST
                zi(lfiloc-1+ (ich-1)*12+3) = 8
                zi(lfiloc-1+ (ich-1)*12+4) = 1
!- POSI_FREQ
                zi(lfiloc-1+ (ich-1)*12+5) = 8
                zi(lfiloc-1+ (ich-1)*12+6) = 1
!- NOM_CMP
                zk8(lficmp-1+ (ich-1)*1000+1) = 'SIXX'
                zk8(lficmp-1+ (ich-1)*1000+2) = 'SIXY'
                zk8(lficmp-1+ (ich-1)*1000+3) = 'SIYY'
                zk8(lficmp-1+ (ich-1)*1000+4) = 'SIXZ'
                zk8(lficmp-1+ (ich-1)*1000+5) = 'SIYZ'
                zk8(lficmp-1+ (ich-1)*1000+6) = 'SIZZ'
!
                zi(lfinbc-1+ich) = 6
!
!
!------------------------------- 'PRES'-----------------------------
!
            else if (nocham(1:4).eq.'PRES') then
!- NOM_CHAM
                zk16(lfinom-1+ich) = nocham
!- NUME_DATASET
                zi(lfinum-1+ich) = 57
!- RECORD 6
                zi(lfipar-1+ (ich-1)*800+200+1) = 1
                zi(lfipar-1+ (ich-1)*800+200+2) = 4
                zi(lfipar-1+ (ich-1)*800+200+3) = 1
                zi(lfipar-1+ (ich-1)*800+200+4) = 15
                zi(lfipar-1+ (ich-1)*800+200+5) = 2
                zi(lfipar-1+ (ich-1)*800+200+6) = 1
!- POSI_ORDRE
                zi(lfiloc-1+ (ich-1)*12+1) = 7
                zi(lfiloc-1+ (ich-1)*12+2) = 4
!- POSI_INST
                zi(lfiloc-1+ (ich-1)*12+3) = 8
                zi(lfiloc-1+ (ich-1)*12+4) = 1
!- POSI_FREQ
                zi(lfiloc-1+ (ich-1)*12+5) = 8
                zi(lfiloc-1+ (ich-1)*12+6) = 1
!- NOM_CMP
                zk8(lficmp-1+ (ich-1)*1000+1) = 'PRES'
!
                zi(lfinbc-1+ich) = 1
            endif
        endif
!
100  end do
!
    call jedema()
!
end subroutine
