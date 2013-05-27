subroutine fetmon(infofe, nbi2, nbi, nbtot, nbsd,&
                  dimgi, ifm, mamoy, lstogi, ifet1,&
                  rang, itps, lpara, option)
!-----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  MONITORING DE ALFETI
!----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    integer :: nbi2, nbtot, nbsd, dimgi, ifm, mamoy, nbi, ifet1, rang, itps
    integer :: option
    logical :: lstogi, lpara
    character(len=24) :: infofe
!
!
! DECLARATION VARIABLES LOCALES
    integer :: ifet2, ifet3, ifet4, ifet5, ifet6, ifet7, i, nbproc, iexist, imon
    character(len=8) :: k8bid
    character(len=20) :: nomopt(11)
    character(len=24) :: nommon
    real(kind=8) :: r1, r2, r3, r1m, r2m, r3m, raux2
!
    call jemarq()
    if ((infofe(11:11).eq.'T') .and. (rang.eq.0) .and. (option.eq.1)) then
! MONITORING INITIAL DE FETI
        write(ifm,*)
        write(ifm,*)&
        '**************************************************'
        write(ifm,'(A13,I4,A1)')'<FETI/ALFETI ',rang,'>'
        write(ifm,'(A20,I4)')'NUMERO D''INCREMENT  ',itps
        write(ifm,'(A20,I4)')'NB SOUS-DOMAINES    ',nbsd
        write(ifm,'(A20,I4)')'NB DE MODES RIGIDES ',dimgi
        write(ifm,*)'POINTS INTERFACE / MAILLAGE / RAPPORT'
        write(ifm,1081)nbi2,nbtot,100.d0* nbi2/nbtot
        if (mamoy .ne. 0) then
            if (lstogi) then
                raux2=(100.d0*dimgi*(nbi+dimgi))/mamoy
            else
                raux2=(100.d0*dimgi*dimgi)/mamoy
            endif
        endif
        write(ifm,1082)raux2
        call jeveuo('&FETI.INFO.STOCKAGE.FVAF', 'L', ifet2)
        call jeveuo('&FETI.INFO.STOCKAGE.FNBN', 'L', ifet3)
        call jeveuo('&FETI.INFO.CPU.FACS', 'L', ifet4)
        call jeveuo('&FETI.INFO.CPU.ASSE', 'L', ifet5)
        call jeveuo('&FETI.INFO.CPU.FACN', 'L', ifet6)
        call jeveuo('&FETI.INFO.CPU.ELEM', 'L', ifet7)
        call jelira('&FETI.INFO.CPU.ELEM', 'LONMAX', nbproc, k8bid)
        write(ifm,*)
        write(ifm,*)'SOUS-DOMAINE   / MATRICE   / FACTORISEE   / NOEUDS'
        do 15 i = 1, nbsd
            write(ifm,1075)i,zi(ifet1+i-1),zi(ifet2+i-1),zi(ifet3+i-1)
15      continue
        write(ifm,*)'--------------------------------------------------'
        write(ifm,1080)zi(ifet1+nbsd),zi(ifet2+nbsd),zi(ifet3+nbsd)
        write(ifm,1083)zi(ifet1+nbsd)/nbsd,zi(ifet2+nbsd)/nbsd,&
        zi(ifet3+nbsd)/nbsd
        write(ifm,*)
        write(ifm,*)'SOUS-DOMAINE   / CPU FACSYM / CPU ASSE /'//&
     &              ' CPU FACNUM'
        r1=0.d0
        r2=0.d0
        r3=0.d0
        r1m=-1.d0
        r2m=-1.d0
        r3m=-1.d0
        do 16 i = 0, nbsd
            write(ifm,1084)i,zr(ifet4+i),zr(ifet5+i),zr(ifet6+i)
            r1=r1+zr(ifet4+i)
            r2=r2+zr(ifet5+i)
            r3=r3+zr(ifet6+i)
            if (zr(ifet4+i) .gt. r1m) r1m=zr(ifet4+i)
            if (zr(ifet5+i) .gt. r2m) r2m=zr(ifet5+i)
            if (zr(ifet6+i) .gt. r3m) r3m=zr(ifet6+i)
16      continue
        write(ifm,*)'--------------------------------------------------'
        write(ifm,1085)r1,r2,r3
        write(ifm,1086)r1m,r2m,r3m
!
        r1=0.d0
        r1m=-1.d0
        write(ifm,*)
        write(ifm,*)'PROCESSEUR     / CPU CALCUL_ELEM '
        do 20 i = 1, nbproc
            write(ifm,1087)i,zr(ifet7+i-1)
            r1=r1+zr(ifet7+i-1)
            if (zr(ifet7+i-1) .gt. r1m) r1m=zr(ifet7+i-1)
20      continue
        write(ifm,*)'-----------------------------------'
        write(ifm,1088)r1
        write(ifm,1089)r1m
        write(ifm,*)&
        '**************************************************'
        else if ((infofe(10:10).eq.'T').and.(rang.eq.0).and.lpara.and.&
    (option.eq.2)) then
! PROFILING MPI
        write(ifm,*)
        write(ifm,*)&
        '**************************************************'
        write(ifm,*)' PROFILING MPI'
        nommon='&FETI.MONITORING.MPI'
        call jeexin(nommon, iexist)
! SI L OBJET NOMMON EXISTE DEJA : ARRET
        call assert(iexist.eq.0)
        call jeveuo(nommon, 'L', imon)
! NBRE D'OPTION DE FETAM
        nomopt(1)='REPARTITION SD'
        nomopt(2)='MPI_COMM_RANK'
        nomopt(3)='MPI_COMM_SIZE'
        nomopt(4)='MPI_REDUCE ENTIER'
        nomopt(5)='MPI_REDUCE REEL'
        nomopt(6)='MPI_ALLREDUCE ENTIER'
        nomopt(7)='MPI_REDUCE 2 REEL'
        nomopt(8)='MPI_GATHERV'
        nomopt(9)='MPI_BCAST VECTEUR'
        nomopt(10)='MPI_BCAST SCALAIRE'
        nomopt(11)='MPI_ALLREDUCE REEL'
!
        r1m=0.d0
        write(ifm,*)'APPELS MPI     /  TEMPS CPU / TEMPS SYS /   TOTAL'
        do 30 i = 1, 11
            r1=zr(imon+2*(i-1))
            r2=zr(imon+2*(i-1))
            r3=r1+r2
            r1m=r1m+r3
            write(ifm,1090)nomopt(i),r1,r2,r3
30      continue
        write(ifm,*)'--------------------------------------------------'
        write(ifm,1091)r1m
        write(ifm,*)&
        '**************************************************'
        write(ifm,*)
        call jedetr(nommon)
    endif
!
! FORMAT
    1075 format(' N ',i4,'     :',i12,' ',i12,' ',i12)
    1080 format('TOTAL       :',i15,' ',i15,' ',i15)
    1081 format(i12,' ',i12,'        ',1pd9.2,' %')
    1082 format('TAILLE (GI + GIT*GI)/MATRICE MOYENNE :',d11.4,' %')
    1083 format('MOYENNE     :',i12,' ',i12,' ',i12)
    1084 format(' N ',i4,'     :     ',d11.4,' ',d11.4,' ',d11.4)
    1085 format('CPU + SYS TOTAL  :',d11.4,' ',d11.4,' ',d11.4)
    1086 format('CPU + SYS LE PIRE:',d11.4,' ',d11.4,' ',d11.4)
    1087 format(' N ',i4,'     :     ',d11.4)
    1088 format('CPU + SYS TOTAL  :',d11.4)
    1089 format('CPU + SYS LE PIRE:',d11.4)
    1090 format(a20,' ',d10.2,' ',d10.2,' ',d10.2)
    1091 format('TOTAL                :',d11.2)
!
    call jedema()
end subroutine
