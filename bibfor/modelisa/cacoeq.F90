subroutine cacoeq(chargz, nomaz)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
!
#include "asterfort/aflrch.h"
#include "asterfort/afrela.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmex.h"
#include "asterfort/cfsuex.h"
#include "asterfort/cncinv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    character(len=*) :: chargz, nomaz
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES)
!
! REALISATION DES LIAISONS LINEAIRES POUR MAILLES QUADRATIQUES
!
! ----------------------------------------------------------------------
!
!
! IN  CHARGE : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  FONREE : FONC OU REEL SUIVANT L'OPERATEUR
!
!
!
!
    integer :: no(3), ii, jj, iret, jnoqu, kk
    complex(kind=8) :: betac, coemuc(3)
    character(len=2) :: typlag
    character(len=4) :: fonree, typcoe
    character(len=8) :: betaf, ddl(3), nono(3), char, noma, type1, type2
    character(len=19) :: lisrel
    character(len=24) :: conoqu, noesup, coninv
    integer :: iexcl, nbexcl
    real(kind=8) :: coemur(3), direct(6), beta
    integer :: idim(3), nbelqu, nmap, jjinv, jjinp, jnoes
    integer :: numail, ityp, iatyma, nutyp
    integer :: suppo1, suppo2, suppo3
    integer :: izone, nzoco, nnoco
    character(len=24) :: defico
    logical(kind=1) :: lreli
    data direct/0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    fonree = 'REEL'
    char = chargz
    noma = nomaz
    betaf = '&FOZERO'
    betac = (0.0d0,0.0d0)
    beta = 0.0d0
    coemuc(1) = (1.0d0,0.0d0)
    coemuc(2) = (-0.5d0,0.0d0)
    coemuc(2) = (-0.5d0,0.0d0)
    coemur(1) = 1.0d0
    coemur(2) = -0.5d0
    coemur(3) = -0.5d0
    idim(1) = 0
    idim(2) = 0
    idim(3) = 0
    typlag = '12'
    typcoe = 'REEL'
    lisrel = '&&CACOEQ.RLLISTE'
    nbelqu = 0
    defico = char(1:8)//'.CONTACT'
    nzoco = cfdisi(defico,'NZOCO')
    nnoco = cfdisi(defico,'NNOCO')
!
! --- ACCES OBJETS
!
    conoqu = defico(1:16)//'.NOEUQU'
!
    call jeexin(conoqu, iret)
    if (iret .eq. 0) then
        nbelqu = 0
    else
        call jeveuo(conoqu, 'L', jnoqu)
        call jelira(conoqu, 'LONUTI', nbelqu)
    endif
!
! --- PAS DE LIAISON SI PAS DE NOEUDS QUADRA
!
    if ((iret.eq.0) .or. (nbelqu.eq.0)) then
        goto 40
    endif
!
! --- CONSTRUCTION DE LA CONNECTIVITE INVERSE
!
    coninv = '&&CACOEQ.CONINV'
    noesup = '&&CACOEQ.NOESUP'
    call cncinv(noma, [0], 0, 'V', coninv)
    call jeveuo(noma//'.TYPMAIL', 'L', iatyma)
!
! --- BOUCLE SUR LES NOEUDS A LIAISONNER
!
    nbelqu = nbelqu/3
    call wkvect(noesup, 'V V I', nnoco, jnoes)
    iexcl = 1
    do 30 ii = 1, nbelqu
        no(1) = zi(jnoqu+3* (ii-1)-1+1)
        no(2) = zi(jnoqu+3* (ii-1)-1+2)
        no(3) = zi(jnoqu+3* (ii-1)-1+3)
        call jenuno(jexnum(noma//'.NOMNOE', no(1)), nono(1))
!
        if (no(2) .eq. 0) then
            lreli = .false.
            goto 30
        else
            lreli = .true.
        endif
!
        call jenuno(jexnum(noma//'.NOMNOE', no(2)), nono(2))
        call jenuno(jexnum(noma//'.NOMNOE', no(3)), nono(3))
!
! --- VERIFICATION QUE LE NOEUD LIE AU NOEUD MILIEU N'EST PAS
! --- EXCLU DU CONTACT (SANS_GROUP_NO)
! --- ON NE FAIT PAS LA LIAISON SI SANS_NOEUD_QUAD='OUI'
!
        do 20 izone = 1, nzoco
            call cfmmex(defico, 'CONT', izone, no(1), suppo1)
            call cfmmex(defico, 'CONT', izone, no(2), suppo2)
            call cfmmex(defico, 'CONT', izone, no(3), suppo3)
            if ((suppo1.eq.1) .or. (suppo2.eq.1) .or. (suppo3.eq.1)) then
                lreli = .false.
                goto 30
            endif
20      continue
!
! --- CAS DES MAILLAGES MIXTES TRIA6/QUAD8
! --- SI NOEUD PARTAGE ENTRE TRIA6/QUAD8: ON STOCKE POUR ELIMINER NOEUD
! --- DU CONTACT (CFSUEX)
!
        call jeveuo(jexatr(coninv, 'LONCUM'), 'L', jjinp)
        nmap = zi(jjinp + no(1)+1-1) -zi(jjinp + no(1)-1)
!
        call jeveuo(jexnum(coninv, no(1)), 'L', jjinv)
        do 25 jj = 1, nmap
            numail = zi(jjinv-1+jj)
            ityp = iatyma - 1 + numail
            nutyp = zi(ityp)
            call jenuno(jexnum('&CATA.TM.NOMTM', nutyp), type1)
!
            if (type1(1:5) .eq. 'QUAD8') then
                do 26 kk = 1, nmap
                    if (jj .ne. kk) then
                        numail = zi(jjinv-1+kk)
                        ityp = iatyma - 1 + numail
                        nutyp = zi(ityp)
                        call jenuno(jexnum('&CATA.TM.NOMTM', nutyp), type2)
!
                        if ((type2(1:5).eq.'TRIA6') .or. (type2(1:5) .eq.'TRIA7') .or.&
                            (type2(1:5).eq.'QUAD9')) then
                            zi(jnoes-1+iexcl) = no(1)
                            iexcl = iexcl + 1
                            goto 25
                        endif
                    endif
!
26              continue
            endif
25      continue
!
! --- RELATION LINEAIRE
!
        ddl(1) = 'DX'
        ddl(2) = 'DX'
        ddl(3) = 'DX'
        call afrela(coemur, coemuc, ddl, nono, idim,&
                    direct, 3, beta, betac, betaf,&
                    typcoe, fonree, typlag, 0.d0, lisrel)
        ddl(1) = 'DY'
        ddl(2) = 'DY'
        ddl(3) = 'DY'
        call afrela(coemur, coemuc, ddl, nono, idim,&
                    direct, 3, beta, betac, betaf,&
                    typcoe, fonree, typlag, 0.d0, lisrel)
        ddl(1) = 'DZ'
        ddl(2) = 'DZ'
        ddl(3) = 'DZ'
        call afrela(coemur, coemuc, ddl, nono, idim,&
                    direct, 3, beta, betac, betaf,&
                    typcoe, fonree, typlag, 0.d0, lisrel)
30  end do
!
    if (lreli) then
        call aflrch(lisrel, char)
        nbexcl = iexcl - 1
        call cfsuex(defico, noesup, nbexcl, nzoco)
        call jedetr(coninv)
        call jedetr(noesup)
    endif
!
40  continue
!
!
    call jedema()
end subroutine
