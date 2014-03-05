subroutine fetskp()
!
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
!    - FONCTION REALISEE:
!       - CREATION DU GRAPHE D'ENTREE DU PARTITIONNEUR
!       - APPEL A METIS OU EXECUTION DE SCOTCH
!       - CREATION DE NOUVEAUX GROUPES DE MAILLES
!----------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
!
    implicit none
!
#include "aster_types.h"
#include "jeveux.h"
#include "asterc/aplext.h"
#include "asterc/fetsco.h"
#include "asterc/getfac.h"
#include "asterc/gtoptk.h"
#include "asterfort/asmpi_comm_jev.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/creaco.h"
#include "asterfort/creagm.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxcadr.h"
#include "asterfort/lxlgut.h"
#include "asterfort/ulnume.h"
#include "asterfort/ulopen.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: nbmama, idco, nbmato, renum2, nbma, nomsdm, masd
    integer :: nbmasd, id,  err, co, renum
    integer ::    numsdm, nmap, i, ima, lrep
    integer :: iulm1, iocc, nocc, ifm, niv, nblien, nbpart, renum3, idma, iulm2
    integer :: rang, nbproc, versco, n1, n2, n3, ier, iaux, iaux2
    integer :: vali(2), iret
    real(kind=8) :: tmps(6)
    character(len=8) :: ma, ktmp, mod, ktmp2, meth, k8nb
    character(len=8) :: kersco
    character(len=24) :: k24b
    character(len=256) :: jnom(4)
    character(len=128) :: rep
    integer(kind=4), pointer :: vedlo(:) => null()
    integer, pointer :: vrenum1(:) => null()
    integer(kind=4), pointer :: vvelo(:) => null()
    mpi_int :: mrank, msize
!
!---------------------------------------------------------------------------------
    call jemarq()
    call infniv(ifm, niv)
!
    nbproc=1
    rang=0
    call asmpi_info(rank=mrank, size=msize)
    rang = to_aster_int(mrank)
    nbproc = to_aster_int(msize)
!
! ********************************************************************
!                       CREATION DU GRAPHE
!
! ------- ON RECUPERE LES DONNEES DU MAILLAGE OU DU MODELE
!
    call getvid(' ', 'MODELE', scal=mod, nbret=err)
    ASSERT(err.eq.1)
    call dismoi('NOM_MAILLA', mod, 'MODELE', repk=ma)

    call dismoi('NB_MA_MAILLA', ma, 'MAILLAGE', repi=nbmato)
    AS_ALLOCATE(vi=vrenum1, size=nbmato)

    nbmato=0
    call jelira(mod//'.MODELE    .LIEL', 'NMAXOC', nocc)
    do iocc = 1, nocc
        call jelira(jexnum(mod//'.MODELE    .LIEL', iocc), 'LONMAX', nbma)
        nbmato=nbmato+nbma-1
    end do
    call wkvect('&&FETSKP.RENUM', 'V V I', nbmato, renum)
    id=1
    do iocc = 1, nocc
        call jelira(jexnum(mod//'.MODELE    .LIEL', iocc), 'LONMAX', nbma)
        call jeveuo(jexnum(mod//'.MODELE    .LIEL', iocc), 'L', idma)
        do ima = 1, nbma-1
            zi(renum-1+id)=zi(idma-1+ima)
! ----- ON VERIFIE QUE LE MODELE NE CONTIENT PAS DE MAILLES TARDIVES
! ----- QUI SONT ESSENTIELLEMENT DES NOEUDS A CE STADE
            if (zi(idma-1+ima) .lt. 0) then
                call utmess('F', 'PARTITION_3')
            endif
            vrenum1(zi(idma-1+ima))=id
            id=id+1
        end do
    end do
!
! ------- CREATION DE LA CONNECTIVITE DES MAILLES
!
    call creaco(nbmato, ma, nblien)
!
! ------ ON RECUPERE LES TABLEAUX CONSTRUITS DANS CREACO
!
    call jeveuo('&&FETSKP.RENUM2', 'L', renum2)
    call jeveuo('&&FETSKP.RENUM3', 'L', renum3)
    call jeveuo('&&FETSKP.CO', 'L', co)
    call jeveuo('&&FETSKP.IDCO', 'L', idco)
    call jeveuo('&&FETSKP.NBMAMA', 'L', nbmama)
!
! ------- ON RECUPERE LE NBRE DE SD ET LE PARTITONNEUR
!
    call getvis(' ', 'NB_PART', scal=nbpart, nbret=err)
    call getvtx(' ', 'METHODE', scal=meth, nbret=err)
!
! -------  UTILISATION DE CONTRAINTES
!
    AS_ALLOCATE(vi4=vvelo, size=nbmato)
    AS_ALLOCATE(vi4=vedlo, size=nblien)
    do ima = 1, nbmato
        vvelo(ima)=1
    end do
    do i = 1, nblien
        vedlo(i)=1
    end do
!
!
! ------- ON IMPRIME LE GRAPH SI PROC 0
!
    if ((meth(1:6).ne.'SCOTCH') .and. (rang.eq.0)) then
        iulm1 = ulnume ()
        if (iulm1 .eq. -1) then
            call utmess('F', 'UTILITAI_81')
        endif
        call ulopen(iulm1, ' ', ' ', 'NEW', 'O')
        write(iulm1,'(I12,I12,I3)')nbmato,nblien/2,11
!
        do ima = 1, nbmato
            write(k8nb,'(''('',I4,''I8'','')'')') 2*(1+zi4(idco-1+ima+&
            1)-1 - zi4(idco-1+ima) ) + 2
            write(iulm1,k8nb)vvelo(ima), (zi4(co-1+i),vedlo(&
            i), i=zi4(idco-1+ima),zi4(idco-1+ima+1)-1)
        end do
!
        call ulopen(-iulm1, ' ', ' ', ' ', ' ')
!
    endif
!
    AS_DEALLOCATE(vi=vrenum1)
    call jedetr('&&FETSKP.NBMAMA')
!
!
! ********************************************************************
!                       LANCEMENT DU LOGICIEL
!
!
!     ************** LANCEMENT DE SCOTCH
!
    if (meth(1:6) .eq. 'SCOTCH') call wkvect('&&FETSKP.NMAP', 'V V S', nbmato, nmap)
    if ((meth(1:6).eq.'SCOTCH') .and. (rang.eq.0)) then
        if (niv .ge. 2) then
            call uttcpu('CPU.FETSKP', 'INIT', ' ')
            call uttcpu('CPU.FETSKP', 'DEBUT', ' ')
        endif
        write(ifm,*) ' '
        write(ifm,*) '***************** SCOTCH *****************'
        write(ifm,*) ' '
        write(ifm,*) ' '
        write(ifm,*) ' * LE NOMBRE DE MAILLES    :',nbmato
        write(ifm,*) ' * LE NOMBRE DE CONNEXIONS :',nblien
        write(ifm,*) ' '
        call fetsco(nbmato, nblien, zi4(co), zi4(idco), nbpart,&
                    zi4(nmap), vedlo(1), vvelo(1), versco, ier)
        n1=versco/10000
        n2=(versco-n1*10000)/100
        n3=versco-n1*10000-n2*100
        kersco(1:8)='........'
        write(kersco(1:2),'(I2)')n1
        write(kersco(4:5),'(I2)')n2
        write(kersco(7:8),'(I2)')n3
        if (niv .ge. 2) then
            call uttcpu('CPU.FETSKP', 'FIN', ' ')
            call uttcpr('CPU.FETSKP', 6, tmps)
            write(ifm,*) ' * TEMPS DE PARTITIONNEMENT  :',tmps(3)
            write(ifm,*) ' '
        endif
        write(ifm,*) '********** FIN SCOTCH ',kersco,' *********'
        if (ier .ne. 0) then
            call utmess('F', 'UTILITAI_56', si=ier)
        endif
        write(ifm,*) ' '
!
!     ************** LANCEMENT DE METIS
!
    else if (rang.eq.0) then
        write(ktmp,'(I4)') nbpart
        write(ktmp2,'(I4)') iulm1
        call lxcadr(ktmp)
        call lxcadr(ktmp2)
        jnom(2)='fort.'//ktmp2
        jnom(3)=ktmp
         call gtoptk('repout', rep, iret)
         if (iret .ne. 0) then
             vali(1) = len(rep)
             call utmess('F', 'EXECLOGICIEL0_24', si=vali(1))
         endif
         lrep = lxlgut(rep)
         if (meth .eq. 'PMETIS  ') then
             jnom(1)=rep(1:lrep)//'/pmetis'
         else if (meth .eq. 'KMETIS  ') then
             jnom(1)=rep(1:lrep)//'/kmetis'
         endif
        call aplext(niv, 3, jnom, err)
    endif
!
    AS_DEALLOCATE(vi4=vedlo)
    AS_DEALLOCATE(vi4=vvelo)
!
!
! ********************************************************************
!                    CREATION DES GROUPES DE MAILLES
!
!
    call wkvect('&&FETSKP.NUMSDM', 'V V I', nbmato, numsdm)
    call wkvect('&&FETSKP.NBMASD', 'V V I', nbpart, nbmasd)
!
! ------- LECTURE DU RESULTAT DU PARTITONNEUR
!
    if (meth(1:6) .ne. 'SCOTCH') then
        if (rang .eq. 0) then
            iulm2 = ulnume ()
            if (iulm2 .eq. -1) then
                call utmess('F', 'UTILITAI_81')
            endif
            lrep=0
            do i = 1, len(ktmp2)
                if (ktmp2(i:i) .ne. ' ') lrep=lrep+1
            end do
            jnom(1)='fort.'//ktmp2(1:lrep)//'.part.'//ktmp
            call ulopen(iulm2, jnom(1), ' ', 'OLD', 'O')
            do ima = 1, nbmato
                read(iulm2,'(I4)')zi(numsdm-1+zi(renum2-1+ima))
            end do
            call ulopen(-iulm2, ' ', ' ', ' ', ' ')
        endif
        k24b='&&FETSKP.NUMSDM'
        call asmpi_comm_jev('BCAST', k24b)
        do ima = 1, nbmato
            iaux=zi(renum2-1+ima)
            iaux2=zi(numsdm-1+iaux)
            zi(nbmasd+iaux2)=zi(nbmasd+iaux2)+1
        end do
    else
        k24b='&&FETSKP.NMAP'
        call asmpi_comm_jev('BCAST', k24b)
        do ima = 1, nbmato
            zi(numsdm-1+zi(renum2-1+ima))=zi4(nmap-1+ima)
            zi(nbmasd+zi(numsdm-1+zi(renum2-1+ima)))= zi(nbmasd+zi(&
            numsdm-1+zi(renum2-1+ima)))+1
        end do
        call jedetr(k24b)
    endif
!
! ------- CREATION DES GROUP_MA
!
    call creagm(nbmato, nbpart, ma, masd)
!
    call jeveuo('&&FETSKP.NOMSDM', 'L', nomsdm)
    do i = 1, nbpart
        write(ifm,*)'LE SOUS DOMAINE ',zk24(nomsdm-1+i),' CONTIENT '&
                       ,zi(nbmasd-1+i),' MAILLES '
    end do

    call jedetr('&&FETSKP.TEMP')
    call jedetr('&&FETSKP.RENUM2')
    call jedetr('&&FETSKP.RENUM3')
    call jedetr('&&FETSKP.IDMASD')
    call jedetr('&&FETSKP.NOMSDM')
    call jedetr('&&FETSKP.MASD')
    call jedetr('&&FETSKP.NBMASD')
    call jedetr('&&FETSKP.NUMSDM')
    call jedetr('&&FETSKP.RENUM')
    call jedetr('&&FETSKP.CO')
    call jedetr('&&FETSKP.IDCO')

    call jedema()
end subroutine
