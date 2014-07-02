subroutine tstobj(ob, perm, resume, sommi, sommr,&
                  lonuti, lonmax, type, iret, ni)
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
!
! BUT : RECUPERER 5 NOMBRES REPRESENTANT UN OBJET JEVEUX
!
! IN: OB    K24     : NOM D'UN OBJET JEVEUX
! IN: PERM  K3 : /OUI/NON
!           NON : ON FAIT LA SOMME BETE DES ELEMENTS DU VECTEUR
!                 => UNE PERMUTATION DU VECTEUR NE SE VOIT PAS !
!           OUI : ON FAIT UNE "SOMME" QUI DONNE UN RESULTAT
!                 DEPENDANT UN PEU DE L'ORDRE DES ELEMENTS DU VECTEUR
!
! OUT: RESUME  I      : VALEUR "RESUMANT" LE CONTENU BINAIRE DE OB
! OUT: SOMMI   I      : SOMME(OB(I)) SI OB EST DE TYPE "I"
! OUT: SOMMR   R      : SOMME(ABS(OB(I))) SI OB EST DE TYPE "R/C"
! OUT: LONUTI  I      : LONUTI (OU SOMME DES LONUTI)
! OUT: LONMAX  I      : LONMAX (OU SOMME DES LONMAX)
!
! OUT: TYPE    K3     : TYPE DES ELEMENTS DE OB :
!                         I/R/C/L/K8/K16/K24/K32/K80
! OUT: IRET    I      : /0 : OK
!                       /1 : NOOK
! OUT: NI      I      : NOMBRE DE VALEURS IGNOREES DANS SOMMR
!
!
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/tstvec.h"
#include "asterfort/wkvect.h"
    character(len=*) :: ob, perm
    character(len=24) :: ob1
    character(len=1) :: xous, typ1
    real(kind=8) :: sommr, sommr2
    integer :: resume, sommi, iret0, lonuti, lonmax
    integer :: sommi2, ltyp, ni
    integer(kind=8) :: sommi3
    integer :: iret, iadm, iadd, long, lon2, iad, kk, nbign
    integer :: nbob2, itrou, iobj
    aster_logical :: contig
    character(len=24) :: k24
    character(len=8) :: stock
    character(len=3) :: type
    character(len=1) :: genr
!
!
    ob1=ob
    call jemarq()
!
!     VALEURS PAR DEFAUT (SI ON SORT AVANT LA FIN) :
    ni=0
    iret=1
    type='XXX'
    lonmax=0
    lonuti=0
    sommi=0
    sommr=0.d0
    resume=0
!
    call jeexin(ob1, iret0)
    if (iret0 .eq. 0) goto 9999
!
    call jelira(ob1, 'TYPE', cval=typ1)
    if (typ1 .eq. 'K') then
        call jelira(ob1, 'LTYP', ltyp)
        if (ltyp .eq. 8) then
            type='K8'
        else if (ltyp.eq.16) then
            type='K16'
        else if (ltyp.eq.24) then
            type='K24'
        else if (ltyp.eq.32) then
            type='K32'
        else if (ltyp.eq.80) then
            type='K80'
        endif
    else
        type=typ1
    endif
!
!
    call jelira(ob1, 'XOUS', cval=xous)
    call jelira(ob1, 'GENR', cval=genr)
!
!
!       - CAS DES OBJETS SIMPLES :
!       --------------------------
    if (xous .eq. 'S') then
!         -- POUR SE PROTEGER DES OBJETS EN COURS DE CREATION :
        call jelira(ob1, 'IADM', iadm)
        call jelira(ob1, 'IADD', iadd)
        if (abs(iadm)+abs(iadd) .eq. 0) goto 9999
!
        if (genr .ne. 'N') then
            call jelira(ob1, 'LONMAX', long)
            call jelira(ob1, 'LONUTI', lon2)
            lonuti=lon2
            lonmax=long
            call jeveuo(ob1, 'L', iad)
        else
            call jelira(ob1, 'NOMMAX', lon2)
            call jelira(ob1, 'NOMUTI', long)
            lonuti=long
            lonmax=lon2
            call wkvect('&&TSTOBJ.PTEUR_NOM', 'V V '//type, long, iad)
            if (type .eq. 'K8') then
                do 51 kk = 1, long
                    call jenuno(jexnum(ob1, kk), zk8(iad-1+kk))
 51             continue
            else if (type.eq.'K16') then
                do 52 kk = 1, long
                    call jenuno(jexnum(ob1, kk), zk16(iad-1+kk))
 52             continue
            else if (type.eq.'K24') then
                do 53 kk = 1, long
                    call jenuno(jexnum(ob1, kk), zk24(iad-1+kk))
 53             continue
            endif
        endif
!
        call tstvec(perm, iad, long, type, sommi,&
                    sommr, nbign)
        ni=ni+nbign
        if (genr .eq. 'N') call jedetr('&&TSTOBJ.PTEUR_NOM')
    endif
!
!
!       - CAS DES COLLECTIONS :
!       -----------------------
    if (xous .eq. 'X') then
        call jelira(ob1, 'NMAXOC', nbob2)
        call jelira(ob1, 'STOCKAGE', cval=stock)
        contig=stock.eq.'CONTIG'
        itrou=0
        lonuti=0
        lonmax=0
        sommi3=0
        sommr=0.d0
        do 2 iobj = 1, nbob2
            call jeexin(jexnum(ob1, iobj), iret0)
            if (iret0 .le. 0) goto 2
!
!            -- POUR SE PROTEGER DES OBJETS EN COURS DE CREATION :
            call jelira(jexnum(ob1, iobj), 'IADM', iadm)
            call jelira(jexnum(ob1, iobj), 'IADD', iadd)
            if (abs(iadm)+abs(iadd) .eq. 0) goto 2
!
            itrou=1
!
            call jelira(jexnum(ob1, iobj), 'LONUTI', lon2)
            call jelira(jexnum(ob1, iobj), 'LONMAX', long)
            lonuti=lonuti+lon2
            lonmax=lonmax+long
            call jeveuo(jexnum(ob1, iobj), 'L', iad)
!
            call tstvec(perm, iad, long, type, sommi2,&
                        sommr2, nbign)
            ni=ni+nbign
            sommi3=sommi3+sommi2
            sommr=sommr+sommr2
            if (.not.contig) call jelibe(jexnum(ob1, iobj))
!
  2     continue
        if (itrou .eq. 0) goto 9999
        write(k24,'(I24)') sommi3
        read(k24(16:24),'(I9)') sommi
    endif
!
!
!     -- SORTIE NORMALE :
    iret=0
!
!     -- POUR L'INSTANT : RESUME= SOMMI
    resume=sommi
!
9999 continue
    call jedema()
end subroutine
