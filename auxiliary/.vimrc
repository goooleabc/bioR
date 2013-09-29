"# xyw's vim setup file
"# date: 2013.09.23

set number
set ts =4
set sw =4
" set backspace = index, eol, start

" show the cursor position always 
set ruler
set autoindent
set fileencodings=gbk,gb2312,utf-8,gb18030
" set termencoding=gbk


" set nocompatible " �ر� vi ����ģʽ
syntax on " �Զ��﷨����
" colorscheme molokai " �趨��ɫ����
" set number " ��ʾ�к�
set cursorline " ͻ����ʾ��ǰ��
" set ruler " ��״̬�����
" set shiftwidth=4 " �趨 << �� >> �����ƶ�ʱ�Ŀ��Ϊ 4
set softtabstop=4 " ʹ�ð��˸��ʱ����һ��ɾ�� 4 ���ո�
" set tabstop=4 " �趨 tab ����Ϊ 4
" set nobackup " �����ļ�ʱ������
" set autochdir " �Զ��л���ǰĿ¼Ϊ��ǰ�ļ����ڵ�Ŀ¼
" filetype plugin indent on " �������
" set backupcopy=yes " ���ñ���ʱ����ΪΪ����
set ignorecase smartcase " ����ʱ���Դ�Сд��������һ�������ϴ�д��ĸʱ�Ա��ֶԴ�Сд����
" set nowrapscan " ��ֹ���������ļ�����ʱ��������
" set incsearch " ������������ʱ����ʾ�������
set hlsearch " ����ʱ������ʾ���ҵ����ı�
" set noerrorbells " �رմ�����Ϣ����
" set novisualbell " �ر�ʹ�ÿ�������������
" set t_vb= " �ÿմ����������ն˴���
" set showmatch " ��������ʱ�����ݵ���ת��ƥ��Ķ�Ӧ����
" set matchtime=2 " ������ת��ƥ�����ŵ�ʱ��
" set magic " ����ħ��
" set hidden " ��������δ������޸�ʱ�л�����������ʱ���޸��� vim ���𱣴�
" set guioptions-=T " ���ع�����
" set guioptions-=m " ���ز˵���
" set smartindent " ��������ʱʹ�������Զ�����
set backspace=indent,eol,start
" " ���趨�ڲ���״̬�޷����˸���� Delete ��ɾ���س���
" set cmdheight=1 " �趨�����е�����Ϊ 1
set laststatus=2 " ��ʾ״̬�� (Ĭ��ֵΪ 1, �޷���ʾ״̬��)
set statusline=\ %<%F[%1*%M%*%n%R%H]%=\ %y\ %0(%{&fileformat}\ %{&encoding}\ %c:%l/%L%)\ 
" " ������״̬����ʾ����Ϣ
" set foldenable " ��ʼ�۵�
" set foldmethod=syntax " �����﷨�۵�
" set foldcolumn=0 " �����۵�����Ŀ��
" setlocal foldlevel=1 " �����۵�����Ϊ
" set foldclose=all " ����Ϊ�Զ��ر��۵� 
" " nnoremap <space> @=((foldclosed(line('.')) < 0) ? 'zc' : 'zo')<CR>
" " �ÿո���������۵�
